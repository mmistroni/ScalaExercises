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
