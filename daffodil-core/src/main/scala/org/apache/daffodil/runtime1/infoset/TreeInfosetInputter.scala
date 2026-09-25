/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.infoset

import java.lang.Boolean as JBoolean
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api
import org.apache.daffodil.api.Daffodil.InfosetInputterEventType
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.TreeInfosetInputter.CapturedEvent
import org.apache.daffodil.runtime1.infoset.TreeInfosetInputter.CapturingOutputter

/**
 * Replays an already-built infoset tree (from a prior build pass) as an
 * InfosetInputter, so write consumes it through the same event-driven
 * unparse() code that consumes any other infoset source. Reuses DIElement's
 * existing walk() (which already skips hidden elements and dispatches
 * simple/complex/array correctly) to precompute the event sequence once, up
 * front. Array boundaries are not captured explicitly: like any other
 * InfosetInputter, they are re-derived from consecutive same-element events.
 */
class TreeInfosetInputter(root: DIDocument) extends api.infoset.InfosetInputter {

  private val events: Array[CapturedEvent] = {
    val capturing = new CapturingOutputter()
    root.walk(capturing)
    capturing.result()
  }

  private var pos = 0

  private def current = events(pos)

  override def getEventType(): InfosetInputterEventType = current match {
    case CapturedEvent.StartDocument => InfosetInputterEventType.StartDocument
    case CapturedEvent.EndDocument => InfosetInputterEventType.EndDocument
    case CapturedEvent.Start(_) => InfosetInputterEventType.StartElement
    case CapturedEvent.End(_) => InfosetInputterEventType.EndElement
  }

  override def getLocalName(): String = current.element.erd.namedQName.local

  override def getSupportsNamespaces = true

  override def getNamespaceURI(): String =
    current.element.erd.namedQName.namespace.toStringOrNullIfNoNS

  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {
    val simple = current.element.asInstanceOf[DISimple]
    // Nilled elements, and any other valueless elements this replay reaches,
    // have nothing to stringify; the caller only uses this text when there's
    // actually a value to set (createElement checks isNilled/isOutputValueCalc
    // itself), so an empty string here is discarded either way.
    if (simple.hasValue) simple.dataValueAsString else ""
  }

  override def isNilled(): JBoolean = {
    val elem = current.element
    if (!elem.erd.isNillable) null
    else JBoolean.valueOf(elem.isNilled)
  }

  override def hasNext(): Boolean = pos < events.length - 1

  override def next(): Unit = pos += 1

  override def fini(): Unit = ()
}

object TreeInfosetInputter {

  private sealed trait CapturedEvent {
    def element: DIElement = this match {
      case CapturedEvent.Start(e) => e
      case CapturedEvent.End(e) => e
      case CapturedEvent.StartDocument | CapturedEvent.EndDocument =>
        Assert.usageError("no element for a document boundary event")
    }
  }

  private object CapturedEvent {
    case object StartDocument extends CapturedEvent
    case object EndDocument extends CapturedEvent
    case class Start(elem: DIElement) extends CapturedEvent
    case class End(elem: DIElement) extends CapturedEvent
  }

  private class CapturingOutputter extends api.infoset.InfosetOutputter {
    private val buf = ArrayBuffer[CapturedEvent]()

    def result(): Array[CapturedEvent] = buf.toArray

    override def reset(): Unit = buf.clear()
    override def startDocument(): Unit = buf += CapturedEvent.StartDocument
    override def endDocument(): Unit = buf += CapturedEvent.EndDocument
    override def startSimple(e: api.infoset.InfosetSimpleElement): Unit =
      buf += CapturedEvent.Start(e.asInstanceOf[DISimple])
    override def endSimple(e: api.infoset.InfosetSimpleElement): Unit =
      buf += CapturedEvent.End(e.asInstanceOf[DISimple])
    override def startComplex(e: api.infoset.InfosetComplexElement): Unit =
      buf += CapturedEvent.Start(e.asInstanceOf[DIComplex])
    override def endComplex(e: api.infoset.InfosetComplexElement): Unit =
      buf += CapturedEvent.End(e.asInstanceOf[DIComplex])
    override def startArray(a: api.infoset.InfosetArray): Unit = ()
    override def endArray(a: api.infoset.InfosetArray): Unit = ()
  }
}
