/**
  * Created by david.chang on 2016/10/30.
  */
class HelloThread extends Thread{
  override def run(): Unit ={
    println("Hello ")
    println("World!")
  }
}

