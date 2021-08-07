package mmo.common.api

import mmo.common.map.{GameMap, Rectangular}

final case class CompactGameMap(
  width: Int,
  height: Int,
  layers: Seq[CompactGameMap.Layer],
  obstaclePositions: Seq[Boolean],
  frontTileIndices: Seq[Boolean]
) extends Rectangular {

  def toGameMap: GameMap = {
    val size = width * height
    val uncompactedLayers = Array.fill[TileIndex](layers.length, size)(TileIndex.empty)

    layers.map(_.uncompact(size)).zipWithIndex.foreach {
      case (layer, layerIndex) =>
      layer.zipWithIndex.foreach {
        case (Some(tileIndex), position) =>
          uncompactedLayers(layerIndex)(position) = tileIndex
        case (None, _) => ()
      }
    }

    GameMap(
      width = width,
      height = height,
      layers = uncompactedLayers.map(_.toArray),
      obstaclePositions = obstaclePositions.toArray,
      frontTileIndices = frontTileIndices.toArray
    )
  }
}

object CompactGameMap {
  sealed trait Layer {
    def uncompact(size: Int): Seq[Option[TileIndex]]
  }

  final case class SparseLayer(tiles: Seq[SparseLayerEntry]) extends Layer {

    override def uncompact(size: Int): Seq[Option[TileIndex]] = {
      val array = Array.fill(size)(Option.empty[TileIndex])
      tiles.foreach {
        case SparseLayerEntry(tileIndex, position) =>
          array(position) = Some(tileIndex)
      }
      array
    }
  }

  final case class SparseLayerEntry(
    tileIndex: TileIndex,
    position: Int
  )

  object SparseLayer {
    def from(array: Array[TileIndex]): SparseLayer =
      SparseLayer(array
        .zipWithIndex
        .filter(!_._1.isEmpty)
        .map((SparseLayerEntry.apply _).tupled)
      )
  }

  final case class RunLengthEncodedLayer(runs: Seq[RunLengthEncodedLayerEntry]) extends Layer {

    override def uncompact(size: Int): Seq[Option[TileIndex]] = {
      val builder = Seq.newBuilder[Option[TileIndex]]
      runs.foreach {
        case RunLengthEncodedLayerEntry(runLength, tileIndex) =>
          val elem = Some(tileIndex).filter(_.asInt >= 0)
          builder ++= Iterator.fill(runLength)(elem)
      }
      builder.result()
    }
  }

  final case class RunLengthEncodedLayerEntry(
    runLength: Int,
    tileIndex: TileIndex
  )

  object RunLengthEncodedLayer {
    def from(array: Array[TileIndex]): RunLengthEncodedLayer =
      RunLengthEncodedLayer(
        runLength(array).map((RunLengthEncodedLayerEntry.apply _).tupled)
      )

    private def runLength[T](xs: Seq[T]): Seq[(Int, T)] = xs match {
      case head +: tail =>
        val (before, after) = tail.span(_ == head)
        (before.length + 1, head) +: runLength(after)
      case Nil => Nil
    }
  }

  def from(gameMap: GameMap): CompactGameMap = {
    val layers: Array[Layer] = gameMap.layers.map { layer =>
      val rle = RunLengthEncodedLayer.from(layer)
      val sparse = SparseLayer.from(layer)
      if (rle.runs.size < sparse.tiles.size) {
        rle
      } else {
        sparse
      }
    }
    CompactGameMap(
      width = gameMap.width,
      height = gameMap.height,
      obstaclePositions = gameMap.obstaclePositions,
      frontTileIndices = gameMap.frontTileIndices,
      layers = layers
    )
  }
}
