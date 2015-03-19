package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.DIDocument
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

/**
 * Iterates an infoset tree, handing out elements one by one in response to pull calls.
 *
 * Assumes that arrays have already been recognized and turned into DIArray nodes.
 */

class InfosetSourceFromTree(doc: InfosetDocument) extends InfosetSource {

  private val root = doc.asInstanceOf[DIDocument].root

  private def folderFunc(diNode: DINode, more: Stream[InfosetEvent]): Stream[InfosetEvent] = {
    val first = Start(diNode)
    val rest = End(diNode) #:: more
    val childrenAndRest = diNode.children.foldRight(rest) { folderFunc }
    val result = first #:: childrenAndRest
    result
  }

  private lazy val stream = folderFunc(root, Stream.Empty)

  private lazy val iterator = stream.toIterator

  override def next = iterator.next

  override def hasNext = iterator.hasNext

  override def toStream = stream
}
