import java.util.Date
import java.text.SimpleDateFormat
import scala.io.StdIn
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.immutable.Seq
import javax.swing.text.StyledEditorKit.BoldAction

class Student(
    var ID: Int,
    var name: String,
    var birDate: Date,
    var gender: Boolean
) {
}

object Table {
    def pad(str: String, length: Int): String = length - str.length match {
        case 0 => str
        case 1 => str + " "
        case _ => pad(" " + str + " ", length)
    }
    /**
      * 生成表格
      *
      * @return
      */
    def make(data: Array[Array[String]], minWidth: Int = 0, bordered: Boolean = true): String = {
        var rows = data.length
        if (rows == 0) return "+"
        var cols = data(0).length
        var getMaxWidth = (i: Int) => data
                .map(row => row(i).length + 2)
                .reduce(Math.max)
        var widths = (0 to cols-1).map(getMaxWidth)
        var totalWidth = widths.reduce((a, b) => a + b)
        if (totalWidth < minWidth) {
            widths = widths.map(e => e * minWidth / totalWidth)
        }
        var divider =
            widths.map(e => List.fill(e)("-").mkString).mkString("+", "+", "+")
        var output = List(divider)
        data.foreach((row) => {
            output = output :+ ((0 to row.length-1).map(
                i => pad(row(i), widths(i))
            ).mkString("|", "|", "|"))
            if (bordered) output = output :+ divider
        })
        if (!bordered) output = output :+ divider
        return output.mkString("\n")
    }
}

class ExitException extends Exception("exit"){}
class QLSyntaxException(s:String) extends Exception(s){}
class QLExecException(s:String) extends Exception(s){}

class Token (
    val pattern: String,
    val content: String,
)

object Tokenlizer {
    def tokenlize(_str: String): List[Token] = {
        val str = _str.split("") :+ " "
        var it = 0
        var status = 0
        var quote = " "
        var nwcontent: List[String] = List()
        val whitespaceReg = "\\s".r
        val quoteReg = "['`]".r
        val operators = Set(
            "+", "-", "*",
            "=", "<", ">",
        )
        val logics = Set(
            "and", "or", "not"
        )
        val bool = Set(
            "true", "false"
        )
        val handlekeyword = Set(
            "insert", "select", "delete", "update",
            "from", "where", "set",
            "limit", "order", "values",
        ) // 句柄
        var tokens: List[Token] = List()
        val breakword = () => {
            if (!nwcontent.isEmpty) {
                var s = nwcontent.mkString
                val lw = s.toLowerCase()
                var pattern = "word"
                if (handlekeyword.contains(lw)) {
                    pattern = "keyword"
                    s = lw
                } else if (logics.contains(lw)) {
                    pattern = "logic"
                    s = lw
                } else if (bool.contains(lw)) {
                    pattern = "number"
                    s = if (lw == "true") "1" else "0"
                }
                    // println(s"${pattern}: ${s}")
                tokens = tokens :+ new Token(pattern, s)
                nwcontent = List()
            }
        }
        while (it < str.length) {
            val ch = str(it)
            status match {
                case 0 => // 普通匹配
                    if (whitespaceReg.findFirstIn(ch) != None) { // 空白符
                        breakword()
                    } else if (quoteReg.findFirstIn(ch) != None) {
                        breakword()
                        quote = ch
                        status = 1
                    } else if (ch == "(" || ch == ")") {
                        breakword()
                        tokens = tokens :+ new Token("bracket", ch)
                    } else if (ch == ",") {
                        breakword()
                        tokens = tokens :+ new Token("comma", ch)
                    } else if (operators.contains(ch)) {
                        breakword()
                        tokens = tokens :+ new Token("operator", ch)
                    } else {
                        nwcontent = nwcontent :+ ch
                    }
                case 1 => // 字符串
                    if (ch == quote) {
                        val s = nwcontent.mkString
                        if (quote == "'") { // 字符串
                            tokens = tokens :+ new Token("string", s)
                        } else { // 字段
                            tokens = tokens :+ new Token("col", s)
                        }
                        nwcontent = List()
                        status = 0
                    } else {
                        nwcontent = nwcontent :+ ch
                    }
            }
            it = it + 1
        }
        if (status == 1) {
            throw new QLSyntaxException("unclosed quote")
        }
        return tokens
    }
}

class QLAST (
    val pattern: String,
    val param: Map[String, String],
    val children: Map[String, QLAST],
) {

}

object Parser {
    val debug = false

    def output(name: String, tokens: List[Token]): Unit = {
        if (!debug) return
        print(s"${name}: [")
        tokens.foreach(t => print(s"[${ t.pattern }, ${ t.content }], "))
        println("]")
    }

    def removeBracket(tokens: List[Token]): List[Token] = {
        var status = 0
        val head = tokens.head
        val tail = tokens.reverse.head
        if (head.pattern != "bracket" || head.content != "(") {
            status |= 1
        }
        if (tail.pattern != "bracket" || tail.content != ")") {
            status |= 2
        }
        if (status == 3) {
            throw new QLSyntaxException("need bracket")
        } else if (status != 0) {
            var loss = "("
            if (status == 2) loss = ")"
            throw new QLSyntaxException(s"bracket doesn't match, need '${ loss }'")
        }
        return tokens.drop(1).dropRight(1)
    }

    def split(_tokens: List[Token], p: Token => Boolean, keep: Boolean = false): List[List[Token]] = {
        var tokens = _tokens
        var ret: List[List[Token]] = List()
        var retnw: List[Token] = List()
        while (!tokens.isEmpty) {
            val token = tokens.head
            tokens = tokens.drop(1)
            if (p(token)) {
                ret = ret :+ retnw
                retnw = List()
                if (keep) {
                    ret = ret :+ List(token)
                }
            } else {
                retnw = retnw :+ token
            }
        }
        if (!retnw.isEmpty) {
            ret = ret :+ retnw
        }
        return ret
    }

    def parseCols(tokens: List[Token]): QLAST = {
        var colset: Map[String, String] = Map()
        val cols = split(tokens, t => t.pattern == "comma")
        var colcnt = 0
        cols.foreach((col) => {
            if (col.isEmpty) {
                throw new QLSyntaxException("need comma in define of cols")
            }
            if (col.length > 1) {
                throw new QLSyntaxException("unexcept comma in define of cols")
            }
            // println(s"col${ colcnt }: ${ col.head.content }")
            colset += (s"col${ colcnt }" -> col.head.content)
            colcnt = colcnt + 1
        })
        colset += ("colcnt" -> colcnt.toString())
        return new QLAST("cols", colset, Map())
    }

    def quoteSplit(_tokens: List[Token]): List[List[Token]] = { // 按括号分解
        var tokens = _tokens
        var ret: List[List[Token]] = List()
        var retnw: List[Token] = List()
        var bl = 0
        while (!tokens.isEmpty) {
            val token = tokens.head
            tokens = tokens.drop(1)
            if (bl == 0) {
                if (token.pattern == "bracket") {
                    if (token.content == ")") {
                        throw new QLSyntaxException("unexcept bracket")
                    }
                    bl = bl + 1
                    retnw = retnw :+ token
                } else {
                    ret = ret :+ List(token)
                }
            } else {
                retnw = retnw :+ token
                if (token.pattern == "bracket") {
                    if (token.content == "(") bl = bl + 1
                    else bl = bl - 1
                    if (bl == 0) {
                        ret = ret :+ retnw
                        retnw = List()
                    }
                }
            }
        }
        if (bl != 0) {
            throw new QLSyntaxException("unexcept bracket")
        }
        return ret
    }

    /**
      * 返回不在括号内的第一个符合条件的token
      *
      * @param _tokens
      * @param p
      * @return
      */
    def quotedIndex(_tokens: List[Token], p: Token => Boolean): Int = {
        val tokens = quoteSplit(_tokens)
        var cnt = 0
        for (token <- tokens) {
            if (token.length == 1 && p(token.head)) return cnt
            cnt = cnt + token.length
        }
        return -1
    }

    def parseParam(_tokens: List[Token]): QLAST = {
        output("parseParam", _tokens)
        var tokens = _tokens
        var info: Map[String, String] = Map()
        var sub: Map[String, QLAST] = Map()
        if (tokens.length == 1) { // 确定是值
            val token = tokens.head
            info += ("type" -> "value")
            info += ("pattern" -> token.pattern)
            info += ("content" -> token.content)
        } else { // 表达式分解
            // [ = , < , > ] , [ + , - ] , *
            var res = quotedIndex(tokens,
                    p => (p.pattern == "operator" &&
                        (p.content == "=" || p.content == ">" || p.content == "<")
                    )
                )
            if (res == -1) { 
                res = quotedIndex(tokens,
                    p => (p.pattern == "operator" &&
                        (p.content == "+" || p.content == "-")
                    )
                )
            }
            if (res == -1) { // 进行乘法分解
                res = quotedIndex(tokens,
                    p => p.pattern == "operator" &&
                        (p.content == "*")
                )
            }
            res match {
                case -1 => // 尝试括号分离
                    if (tokens.exists(t => t.pattern == "bracket")) {
                        throw new QLSyntaxException("invalid experssion")
                    }
                    info += ("type" -> "expr")
                    info += ("opt" -> "+")
                    sub += ("expr" -> parseParam(removeBracket(tokens)))
                case 0 =>
                    val opt = tokens.head
                    tokens = tokens.drop(1)
                    info += ("type" -> "expr")
                    if (debug) println(opt.content)
                    info += ("opt" -> opt.content)
                    sub += ("expr" -> parseParam(tokens))
                case _ =>
                    val hs = tokens.splitAt(res)
                    val lhs = hs._1
                    val rhs = hs._2.drop(1)
                    val opt = hs._2.head
                    if (debug) println(opt.content)
                    info += ("type" -> "expr-expr")
                    info += ("opt" -> opt.content)
                    sub += ("lhs" -> parseParam(lhs))
                    sub += ("rhs" -> parseParam(rhs))
            }
        }
        return new QLAST("param", info, sub)
    }

    def parseParams(_tokens: List[Token]): QLAST = {
        val tokens = removeBracket(_tokens)
        var paramset: Map[String, QLAST] = Map()
        var paramcnt = 0
        if (!tokens.isEmpty) {
            val params = split(tokens, t => t.pattern == "comma")
            if (debug) println(params)
            params.foreach((param) => {
                if (param.isEmpty) {
                    throw new QLSyntaxException("need comma in arguments")
                }
                paramset += (s"param${ paramcnt }" -> parseParam(param))
                paramcnt = paramcnt + 1
            })
        }
        return new QLAST("params", Map("paramcnt" -> paramcnt.toString()), paramset)
    }

    def parseSetter(tokens: List[Token], equalOnly: Boolean = false): QLAST = { // 表达式
        output("parserSetter", tokens)
        // 从 '=' 处切开
        val res = quotedIndex(tokens, t => t.pattern == "operator" && t.content == "=")
        if (debug) println(res)
        if (res != 1) {
            throw new QLSyntaxException("invalid expression")
        }
        val hs = tokens.splitAt(res)
        val receiver = hs._1.head
        // val opt = hs._2.head
        // if (equalOnly && opt.content != "=") {
        //     throw new QLSyntaxException("invalid expression")
        // }
        val value = hs._2.drop(1)
        return new QLAST("setter", Map("col" -> receiver.content), Map("value" -> parseParam(value)))
    }

    def parseSetters(tokens: List[Token]): QLAST = {
        var exprset: Map[String, QLAST] = Map()
        var exprcnt = 0
        val exprs = split(tokens, t => t.pattern == "comma")
        exprs.foreach((expr) => {
            if (expr.isEmpty) {
                throw new QLSyntaxException("need comma")
            }
            exprset += (s"setter${ exprcnt }" -> parseSetter(expr))
            exprcnt = exprcnt + 1
        })
        return new QLAST("setters", Map("settercnt" -> exprcnt.toString()), exprset)
    }

    def parseFilters(tokens: List[Token]): QLAST = { // expr, expr - expr, expr - filters
        output("parseFilters: ", tokens)
        var filterset: Map[String, QLAST] = Map()
        var opt = "#"
        // 优先级为: or - and - not
        var res = quotedIndex(tokens, t => t.pattern == "logic" && t.content == "or")
        if (res == -1) {
            res = quotedIndex(tokens, t => t.pattern == "logic" && t.content == "and")
        }
        if (res == -1) {
            res = quotedIndex(tokens, t => t.pattern == "logic" && t.content == "not")
        }
        if (res == -1) { // 没有逻辑运算符
            if (tokens.exists(t => t.pattern == "bracket")) { // 尝试分解括号
                opt = "@"
                filterset += ("lhs" -> parseFilters(removeBracket(tokens)))
            } else {
                filterset += ("lhs" -> parseParam(tokens))
            }
        } else {
            val hs = tokens.splitAt(res)
            val lhs = hs._1
            val rhs = hs._2.drop(1)
            opt = hs._2.head.content
            if (lhs.isEmpty) {
                if (opt != "not") {
                    throw new QLSyntaxException("illegal experssion")
                } else {
                    filterset += ("lhs" -> parseFilters(rhs))
                }
            } else {
                filterset += ("lhs" -> parseFilters(lhs))
            }
            filterset += ("rhs" -> parseFilters(rhs))
        }
        return new QLAST("filter", Map("opt" -> opt), filterset)
    }

    def parseRoot(_tokens: List[Token]): QLAST = {
        output("parseRoot: ", _tokens)
        var tokens = _tokens
        val getHead = () => {
            val ret = tokens.head
            if (debug) println(s"${ ret.pattern }: ${ ret.content }")
            tokens = tokens.drop(1)
            ret
        }
        val split = (p: Token => Boolean, err: String) => {
            var colTokens: List[Token] = List()
            var find = false
            while (!tokens.isEmpty && !find) {
                val token = getHead()
                if (p(token)) find = true
                else colTokens = colTokens :+ token
            }
            if (!find && !err.isEmpty()) {
                throw new QLSyntaxException(err)
            }
            colTokens
        }
        val pattern = getHead().content.toLowerCase()
        var param: Map[String, String] = Map()
        var children: Map[String, QLAST] = Map()
        pattern match {
            case "show" =>
                val word = getHead().content.toLowerCase()
                if (!tokens.isEmpty) { // like
                    val subopt = getHead()
                    if (subopt.content == "like") {
                        param += ("like" -> getHead().content)
                    } else {
                        throw new QLSyntaxException(
                            "unrecognize word: " + subopt.content + "\n" + 
                            "maybe you want to use like (to filter)"
                        )
                    }
                }
                param += ("key" -> word)
            case "insert" => // insert into ... values( ... )
                if (getHead().content != "into") {
                    throw new QLSyntaxException(
                        "need into to specific table to insert"
                    )
                }
                val table = getHead().content
                if (table == "(") {
                    throw new QLSyntaxException(
                        "need specific table to insert"
                    )
                }
                param += ("into" -> table)
                val colTokens = split((token) =>
                    token.pattern == "keyword" && token.content == "values",
                    "need keyword values to specfic insert item"
                )
                output("insert.cols", colTokens)
                val valueTokens = tokens
                // if (colTokens.length != valueTokens.length) {
                //     throw new QLSyntaxException(
                //         "the number of value doesn't match cols in values()"
                //     )
                // }
                output("insert.values", valueTokens)
                children += ("cols" -> parseCols(removeBracket(colTokens)))
                children += ("values" -> parseParams(valueTokens))
            case "select" => // select ... from ... [where ...] [order by ...]
                // 字段
                val colTokens = split((token) => 
                    token.pattern == "keyword" && token.content == "from",
                    "need keyword from to specfic table to select"
                )
                output("select.cols", colTokens)
                children += ("cols" -> parseCols(colTokens))
                // 表
                val fromToken = getHead()
                param += ("from" -> fromToken.content)
                // 尝试规约 order by
                val whereTokens = split((token) => 
                    token.pattern == "keyword" && token.content == "order", ""
                )
                if (!whereTokens.isEmpty) {
                    output("select.where", tokens)
                    if (whereTokens.head.content != "where") {
                        throw new QLSyntaxException(
                            "unexpect token: " + whereTokens.head.content
                        )
                    }
                    whereTokens.drop(1)
                    children += ("where" -> parseFilters(whereTokens))
                }
                // order by:
                if (!tokens.isEmpty) {
                    val byToken = getHead()
                    if (byToken.content != "by") {
                        throw new QLSyntaxException(
                            "unexcept keyword 'order', maybe you want to use order by"
                        )
                    }
                    val colToken = getHead()
                    param += ("orderby" -> colToken.content)
                    var asc = if (!tokens.isEmpty) {
                        val orderToken = getHead()
                        if (!tokens.isEmpty) {
                            throw new QLSyntaxException(
                                "unexcept token:" + tokens.head.content 
                            )
                        }
                        orderToken.content.toLowerCase match {
                            case "asc" => true
                            case "desc" => false
                            case _ =>
                                throw new QLSyntaxException(
                                    "unexcept token:" + tokens.head.content 
                                )
                        }
                    } else true
                    param += ("order" -> (if (asc) "asc" else "desc"))
                }
            case "update" => // update ... set ... where ...
                // 表
                val fromTokens = split((token) => 
                    token.pattern == "keyword" && token.content == "set",
                    "need keyword from to specfic table to update"
                )
                param += ("from" -> fromTokens.head.content)
                val exprTokens = split((token) => 
                    token.pattern == "keyword" && token.content == "where", ""
                )
                children += ("setters" -> parseSetters(exprTokens))
                if (!tokens.isEmpty) {
                    children += ("where" -> parseFilters(tokens))
                }
            case "delete" => // delete from ... where
                split((token) => 
                    token.pattern == "keyword" && token.content == "from",
                    "need keyword from to specfic table delete from"
                )
                val fromTokens = split((token) => 
                    token.pattern == "keyword" && token.content == "where", ""
                )
                param += ("from" -> fromTokens.head.content)
                if (!tokens.isEmpty) {
                    children += ("where" -> parseFilters(tokens))
                }
            case "source" =>
                if (tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "need the path of source file"
                    )
                }
                val path = getHead()
                if (path.pattern != "string" || !tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "path error, use single string to identity the source file"
                    )
                }
                param += ("path" -> path.content)
            case "help" =>
                if (!tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "help has no subexpression"
                    )
                }
            case "exit" => 
                if (!tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "exit has no subexpression"
                    )
                }
            case "truncate" =>
                if (tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "need specific table to truncate"
                    )
                }
                val table = getHead()
                if (!tokens.isEmpty) {
                    throw new QLSyntaxException(
                        "unexcept token: " + tokens.head.content
                    )
                }
                param += ("table" -> table.content)
            case _ => 
                throw new QLSyntaxException(
                    "unexcept word: " + pattern + "\n"
                )
        }
        if (debug) println("parse finish")
        return new QLAST(pattern, param, children)
    }
    /**
      *
      * @param str
      * @return
      */
    def parse(str: String): QLAST = {
        val tokens = Tokenlizer.tokenlize(str)
        return parseRoot(tokens)
    }
}

class StudentManager {
    var data: List[Student] = List()

    def show(expr: QLAST): String = expr.param.getOrElse("key", "") match {
        case "databases" => Table.make(
            Array(
                Array("databases"),
                Array("StudentManager(selected)")
            ), 
        )
        case "tables" => Table.make(
            Array(
                Array("tables"),
                Array("student")
            ), 
        )
        case key => throw new QLExecException("invaild key to show: " + key)
    }

    def truncate(expr: QLAST): Unit = expr.param.getOrElse("table", "") match {
        case "student" => data = List()
        case key => throw new QLExecException(key + " is not a table of StudentManager")
    }

    def showHelp(): String = {
        //  |    导出: SELECT * INTO OUTFILE 'removejava.ql';
        return ("""本系统使用简单sql进行操作
            |操作示例:
            |    插入: INSERT into student (ID, name, birDate, gender) values (12, '神必人', '1999-01-01', True);
            |    查找: SELECT ID, name, birDate from student WHERE gender = True order by birDate desc;
            |    删除: DELETE from student WHERE ID > 114514;
            |    修改: UPDATE student set ID = ID + 1 WHERE name = '没有人';
            |    执行ql文件: SOURCE 'init.sql';
            |    清空: TRUNCATE student;
            |    退出: exit;
            |发挥你的想象力吧！""".stripMargin
        )
    }

    def calvalue(qlAST: QLAST, stu: Student = new Student(0, "", new Date, false)): String = {
        // println("calvalue: ", qlAST.pattern, qlAST.param, qlAST.children)
        if (qlAST.pattern != "param") {
            throw new QLExecException("inner error")
        }
        val qtype = qlAST.param.getOrElse("type", "#")
        val opt = qlAST.param.getOrElse("opt", "#")
        qtype match {
            case "value" => 
                val p = qlAST.param.getOrElse("pattern", "#")
                val v = qlAST.param.getOrElse("content", "#")
                if (p == "string") v
                else v match {
                    case "ID" => stu.ID.toString()
                    case "name" => stu.name
                    case "birDate" =>
                        val sdf = new SimpleDateFormat("yyyy-MM-dd")
                        sdf.format(stu.birDate)
                    case "gender" => if (stu.gender) "1" else "0"
                    case _ => v
                }
            case "expr-expr" =>
                val olhs = calvalue(qlAST.children.getOrElse("lhs", qlAST), stu)
                val orhs = calvalue(qlAST.children.getOrElse("rhs", qlAST), stu)
                // println(olhs, orhs)
                if (opt == "=") return if (olhs == orhs) "1" else "0"
                val lhs = olhs.toInt
                val rhs = orhs.toInt
                opt match {
                    case "<" => if (lhs < rhs) "1" else "0"
                    case ">" => if (lhs > rhs) "1" else "0"
                    case "+" => (lhs + rhs).toString()
                    case "-" => (lhs - rhs).toString()
                    case "*" => (lhs * rhs).toString()
                    case _ => 
                        throw new QLExecException("illegal opt: " + opt)
                }
            case "expr" =>
                val expr = calvalue(qlAST.children.getOrElse("expr", qlAST), stu)
                opt match {
                    case "+" => expr.toInt.toString()
                    case "-" => (-expr.toInt).toString()
                    case _ => 
                        throw new QLExecException("illegal opt: " + opt)
                }
            case _ => throw new QLExecException("unknown value type: " + qtype)
        }
    }

    // def getCols(qlAST: QLAST, check: Int = 0):  {

    // }

    def updateValue(setters: List[(String, QLAST)], stu: Student = new Student(0, "", new Date, false)): Student = {
        setters.foreach((setter) => {
            val col = setter._1
            val value = calvalue(setter._2, stu)
            col match {
                case "ID" => stu.ID = value.toInt
                case "name" => stu.name = value
                case "birDate" =>
                    val sdf = new SimpleDateFormat("yyyy-MM-dd")
                    stu.birDate = sdf.parse(value)
                case "gender" => stu.gender = (value.toInt != 0)
            }
            // println(col, value)
        })
        // println(stu.ID)
        return stu
    }

    def insert(expr: QLAST): Boolean = {
        if (expr.param.getOrElse("into", "") != "student") {
            throw new QLExecException(
                "unknown table: " + expr.param.getOrElse("into", "")
            )
        }
        val cols = expr.children.getOrElse("cols", expr)
        val values = expr.children.getOrElse("values", expr)
        // println(cols.param.getOrElse("colcnt", "114514"))
        val colcnt = cols.param.getOrElse("colcnt", "114514").toInt
        val paramcnt = values.param.getOrElse("paramcnt", "114514").toInt
        if (colcnt != paramcnt) {
            throw new QLExecException(
                "the number of value doesn't match cols in values()"
            )
        }
        var setters: List[(String, QLAST)] = List()
        var it = 0
        while (it < colcnt) {
            val col = cols.param.getOrElse(s"col${ it }", "114514")
            val value = values.children.getOrElse(s"param${ it }", expr)
            setters = setters :+ (col, value)
            it = it + 1
        }
        data = data :+ updateValue(setters)
        return true
    }

    def judge(expr: QLAST, stu: Student): Boolean = {
        // println("judge: ", expr.pattern, expr.param, expr.children)
        if (expr.pattern != "filter") {
            throw new QLExecException("inner error")
        }
        val opt = expr.param.getOrElse("opt", "#")
        val lhs = expr.children.getOrElse("lhs", expr)
        val rhs = expr.children.getOrElse("rhs", expr)
        return opt match {
            case "#" => calvalue(lhs, stu).toInt != 0
            case "@" => judge(lhs, stu)
            case "not" => !judge(lhs, stu)
            case "and" => judge(lhs, stu) && judge(rhs, stu)
            case "or" => judge(lhs, stu) || judge(rhs, stu)
            case _ => throw new QLExecException("unknown operation: " + opt)
        }
    }

    def update(expr: QLAST, stu: Student): Int = {
        return 0
    }

    def find(qlAST: QLAST): Array[Array[String]] = {
        if (qlAST.param.getOrElse("from", "") != "student") {
            throw new QLExecException(
                "unknown table: " + qlAST.param.getOrElse("into", "")
            )
        }
        // 解析字段
        val cols = qlAST.children.getOrElse("cols", qlAST)
        val colcnt = cols.param.getOrElse("colcnt", "114514").toInt
        var it = 0
        var collist: Array[String] = Array()
        while (it < colcnt) {
            val col = cols.param.getOrElse(s"col${ it }", "114514")
            if (col == "*") {
                collist = collist :++ Array("ID", "name", "birDate", "gender")
            } else {
                collist = collist :+ col
            }
            it = it + 1
        }
        var ret: Array[Array[String]] = Array(collist)
        val judgeExpr = qlAST.children.getOrElse("where", qlAST)
        val ordercol = qlAST.param.getOrElse("orderby", "#")
        val sortedData = if (ordercol != "#") {
            val temp = data.sortWith((a: Student, b: Student) => ordercol match {
                case "ID" => a.ID < b.ID
                case "name" => a.name.compareTo(b.name) < 0
                case "birDate" => a.birDate.compareTo(b.birDate) < 0
                case "gender" => a.gender < b.gender 
                case _ => throw new QLExecException(
                    s"${ ordercol } is not a col of table student"
                )
            })
            val order = qlAST.param.getOrElse("order", "asc")
            if (order == "asc") temp
            else temp.reverse 
        } else data
        sortedData.foreach(item => {
            if (judgeExpr.pattern == "select" || judge(judgeExpr, item)) { // 序列化?
                var dat: Array[String] = Array()
                collist.foreach(col => {
                    dat = dat :+ (col match {
                        case "ID" => item.ID.toString()
                        case "name" => item.name.toString()
                        case "birDate" =>
                            val sdf = new SimpleDateFormat("yyyy-MM-dd")
                            sdf.format(item.birDate)
                        case "gender" => if (item.gender) "True" else "False"
                        case _ => throw new QLExecException(
                            s"${ col } is not a col of table student"
                        )
                    })
                })
                ret = ret :+ dat
            }
        })
        return ret
    }

    def delete(qlAST: QLAST): Int = {
        if (qlAST.param.getOrElse("from", "") != "student") {
            throw new QLExecException(
                "unknown table: " + qlAST.param.getOrElse("into", "")
            )
        }
        val judgeExpr = qlAST.children.getOrElse("where", qlAST)
        val res = data.filter(t => !(qlAST.pattern == "delete" || judge(qlAST, t)))
        var affair = data.length - res.length
        data = res
        return affair
    }

    def modify(qlAST: QLAST): Int = {
        var setters: List[(String, QLAST)] = List()
        val updateExpr = qlAST.children.getOrElse("setters", qlAST)
        val settercnt = updateExpr.param.getOrElse("settercnt", "114514").toInt
        var it = 0
        while (it < settercnt) {
            val setterExpr = updateExpr.children.getOrElse(s"setter${ it }", updateExpr)
            val col = setterExpr.param.getOrElse("col", "114514")
            val value = setterExpr.children.getOrElse("value", setterExpr)
            setters = setters :+ (col, value)
            it = it + 1
        }
        val judgeExpr = qlAST.children.getOrElse("where", qlAST)
        var affair = 0
        data = data.map(item => {
            if (judgeExpr.pattern == "update" || judge(judgeExpr, item)) { // 序列化?
                affair = affair + 1
                updateValue(setters, item)
            } else item
        })
        return affair
    }

    def output(): Unit =  {
        data.foreach(s => println(s.toString()))
    }

    def exec(rawqls: String): Unit = {
        var qls = rawqls.split(";")
        qls.foreach(ql => {
            var info = execSingle(ql)
            println(info)
        })
    }

    def execFromSource(expr: QLAST): String = {
        val path: String = expr.param.getOrElse("path", "")
        var text = ""
        for (line <- Source.fromFile(path, "utf-8").getLines()) {
            text = text + line
        }
        exec(text)
        return s"exec ${path} over"
    }

    def execSingle(ql: String): String = {
        // println(ql)
        val qlAST = Parser.parse(ql)
        qlAST.pattern match {
            case "show" => show(qlAST)
            case "insert" =>
                var res = insert(qlAST)
                if (res) return "insert success"
                else return "insert failed"
            case "select" =>
                val res = find(qlAST)
                return Table.make(res)
            case "update" =>
                val res = modify(qlAST)
                return s"update ${ res } row${ if (res > 1) "s" else "" } in student"
            case "delete" =>
                val res = delete(qlAST)
                return s"delete ${ res } row${ if (res > 1) "s" else "" } from student"
            case "truncate" =>
                truncate(qlAST)
                return "table student truncated"
            case "source" => execFromSource(qlAST)
            case "help" => showHelp()
            case "exit" => throw new ExitException
            case _ => throw new QLExecException("unavailible operataion")
        }
    }

    def App(opt: Int): Unit = {
        opt match {
            case 0 =>
                println("请选择操作(键入7可进入ql操作模式)")
                println(Table.make(Array(
                    Array("1 插入"),
                    Array("2 查找"),
                    Array("3 删除"),
                    Array("4 修改"),
                    Array("5 输出"),
                    Array("6 退出"),
                    Array("7 进入高级模式"),
                ), 40, false))
                App(1)
            case 1 => 
                print("simple> ")
                val input = StdIn.readInt()
                input match {
                    case 7 =>
                        println("欢迎来到高级操作模式, 键入help;可以查看帮助")
                        App(2)
                    case _ =>
                        input match {
                            case 1 => 
                                println("插入学生信息, 请依次输入ID, 名称, 出生日期(yyyy-MM-dd), gender, 用逗号或空格分割")
                                val info = StdIn.readLine().split("[ ,]+").map(t => t.trim())
                                try {
                                    val ID = info(0).toInt
                                    val name = info(1)
                                    val birDate = info(2)
                                    val gender = if (info(3) == "男") "true" else "false"
                                    val ql = s"insert into student (ID, name, birDate, gender) values (${ ID }, '${ name }', '${ birDate }', ${ gender });"
                                    println(ql)
                                    exec(ql)
                                } catch {
                                    case _: Throwable => println("格式错误")
                                }
                            case 2 =>
                                println("按姓名查找学生信息, 请输入姓名")
                                val name = StdIn.readLine().trim()
                                val ql = s"select * from student where name = '${ name }' order by ID;"
                                println(ql)
                                exec(ql)
                            case 3 => 
                                println("按姓名删除学生信息, 请输入姓名")
                                val name = StdIn.readLine().trim()
                                val ql = s"delete from student where name = '${ name }';"
                                println(ql)
                                exec(ql)
                            case 4 => 
                                println("修改学生出生日期, 请依次输入姓名和出生日期, 用逗号分割")
                                val info = StdIn.readLine().split("[ ,]+").map(t => t.trim())
                                try {
                                    val name = info(0)
                                    val birDate = info(1)
                                    val ql = s"update student set birDate = '${ birDate }' where name = '${ name }';"
                                    println(ql)
                                    exec(ql)
                                } catch {
                                    case _: Throwable => println("格式错误")
                                }
                            case 5 => 
                                println("输出所有学生信息")
                                val ql = s"select * from student order by ID;"
                                println(ql)
                                exec(ql)
                            case 6 =>
                                println("退出系统")
                                val ql = s"exit;"
                                println(ql)
                                try {
                                    exec(ql)
                                } catch {
                                    case ex: ExitException => 
                                        println("query end")
                                        return
                                    case ex: QLSyntaxException => 
                                        println("ql syntax error:\n" + ex.getMessage())
                                    case ex: QLExecException =>
                                        println("ql exec error:\n" + ex.getMessage())
                                }
                        }
                        App(1)
                }
            case 2 =>
                print("curd> ")
                var input = ""
                while (!input.endsWith(";")) {
                    input = (input + StdIn.readLine()).trim()
                    if (!input.endsWith(";")) {
                        print("    > ")
                    }
                }
                try {
                    exec(input)
                } catch {
                    case ex: ExitException => 
                        println("query end")
                        return
                    case ex: QLSyntaxException => 
                        println("ql syntax error:\n" + ex.getMessage())
                    case ex: QLExecException =>
                        println("ql exec error:\n" + ex.getMessage())
                }
                App(2)
        }
    }
}

object SMTest {
    def main(args: Array[String]): Unit = {
        new StudentManager().App(0)
    }
}