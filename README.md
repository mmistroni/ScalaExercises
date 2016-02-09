# ScalaExercises

This project contains various Scala Exercises from Project Euler, s-99  problems etc
class CodeEvalSuite{

  
  def createHuffmanFrequencies(textLine:String):List[(Char, Int)] = {
    textLine.foldLeft(Map[Char, Int]())((acc, currentChar) => {
                           val currCount = acc.getOrElse(currentChar, 0) + 1
                           acc + (currentChar -> currCount)
        }).toList
  }
  
  def createQueue(inputList:List[(Char, Int)]): PriorityQueue[HuffNode] = {
    
    val huffmanQueue = new PriorityQueue[HuffNode]()(Ord).reverse
    for ((ch, frequency) <- inputList) huffmanQueue.enqueue(new HuffNode(ch.toString, frequency, Empty, Empty ))
    huffmanQueue
  }
  
  
  
  def generateHuffmanSequence(queue:PriorityQueue[HuffNode]):PriorityQueue[HuffNode] = {
    if (queue.size == 1)
      queue
    else {
      val firstNode = queue.dequeue()
      println(s"${firstNode.id}={firstNode.val}")
      val secondNode = queue.dequeue()
      println(s"${firstNode.id}={firstNode.val}")
      
      val newNode = new HuffNode(s"${firstNode.id}.${secondNode.id}",
                              firstNode.value + secondNode.value,
                              firstNode, secondNode)
      queue.enqueue(newNode)
      generateHuffmanSequence(queue)
      }
  }
    
  
  def huffmanCodeFor(item:String, root:HuffNode, acc:String):String = {
      //println("Visiting:" + root.id)
      if (root.id == item) acc
      else {
        if (root == Empty) // we havent found it
          return ""
        else {
          val res = huffmanCodeFor(item, root.left, acc + "0")
          if (res == "") 
            huffmanCodeFor(item, root.right, acc + "1")
          else res
        }
      }
    
  }
  
  
  @Test
  def testHuffmann(){
    
    val testString = "ilovecodeeval" //"abc"
    
    val freqList = createHuffmanFrequencies(testString)
    println(s"FreqList is:$freqList")
    val priorityQueue = createQueue(freqList)
    
    /**
    println(priorityQueue.dequeue())
    println(priorityQueue.dequeue())
    println(priorityQueue.dequeue())
    **/
    val huffmanQueue = generateHuffmanSequence(priorityQueue)
    val topNode = huffmanQueue.dequeue()
    //println(topNode)
    //println(topNode.left)
    //println(topNode.right)
    
    testString.sorted.distinct.foreach(item => println(s"$item:${huffmanCodeFor(item.toString, topNode, "")}"))
      
    
    //println("A=" + huffmanCodeFor("a", topNode, ""))
    //println("B=" + huffmanCodeFor("b", topNode, ""))
    //println("C=" + huffmanCodeFor("c", topNode, ""))
    
  }
  
  
  
  
  
}



import org.junit._

import Assert._
import Baristax._
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.testkit.{TestKit, TestActorRef, ImplicitSender}
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout

class SimpleTest extends TestKit(ActorSystem("testSystem")) with ImplicitSender{

  
  @Test @Ignore def testDownloaderSynchronously(){
    
    val downloader = TestActorRef[Downloader]
  
    implicit val timeout = Timeout(5 seconds)                                                      
    val future =  downloader ? DownloadFile("Foo")
    
    val result = Await.result(future, 1 second)
    assertEquals(FileContent("|CIX23|20911|4|COMPANY DATA|EDGAR/data/files"), result)
    
  
  }
    /**
    val retriever = TestActorRef(Props(classOf[IndexRetriever], downloader))
    val sink = TestActorRef[EdgarFileSink]
    val sink2 = system.actorOf(Props[EdgarFileSink])
    val fileManager = TestActorRef(Props(classOf[EdgarFileManager], downloader, sink))
    val processor = TestActorRef(Props(classOf[IndexProcessor], fileManager))
    val master = TestActorRef(Props(classOf[EdgarMaster], retriever, processor,
                                                          fileManager))
    **/
    
  @Test def testDownloaderAsync(){
    
    val downloader = TestActorRef[Downloader]
  
    within (1000 millis) {
      downloader ! DownloadFile("Test")
      expectMsg(FileContent("|CIX23|20911|4|COMPANY DATA|EDGAR/data/files"))
    }
  
  }
  
  
  
  
  
  
  
  
}
