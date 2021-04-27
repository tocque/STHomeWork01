call scalac -d ./class SMTest.scala
cd class
call jar -cvfe main.jar SMTest .
move main.jar ../main.jar