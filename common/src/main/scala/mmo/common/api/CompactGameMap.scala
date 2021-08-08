package mmo.common.api

import mmo.common.map.{GameMap, Rectangular}

import scala.collection.immutable.ArraySeq

final case class CompactGameMap(
  width: Int,
  height: Int,
  layers: Array[CompactArray],
  obstaclePositions: CompactArray,
  frontTileIndices: CompactArray
) extends Rectangular {

  def toGameMap: GameMap = {
    val size = width * height

    // TODO: do not hardcode this
    val tileCount = 256

    val uncompactedLayers = layers
      .map(_.uncompact(size))
      .map(_.map(TileIndex.fromOption).toArray)

    val uncompactedObstaclePositions = obstaclePositions
      .uncompact(size)
      .map(_.fold(false)(_ > 0))
      .toArray

    val uncompactedFrontTileIndices = frontTileIndices
      .uncompact(tileCount)
      .map(_.fold(false)(_ > 0))
      .toArray

    GameMap(
      width = width,
      height = height,
      layers = uncompactedLayers,
      obstaclePositions = uncompactedObstaclePositions,
      frontTileIndices = uncompactedFrontTileIndices,
    )
  }
}

object CompactGameMap {

  def from(gameMap: GameMap): CompactGameMap = {
    val layers = gameMap.layers.map { layer =>
      CompactArray.chooseSmallest(ArraySeq.unsafeWrapArray(layer.map(_.toOption)), TileIndex.empty.asInt)
    }
    val obstaclePositions = CompactArray.chooseSmallest(
      data = ArraySeq.unsafeWrapArray(gameMap.obstaclePositions.map {
        case true => Some(1)
        case false => None
      }),
      emptyValue = 0
    )
    val frontTileIndices = CompactArray.chooseSmallest(
      data = ArraySeq.unsafeWrapArray(gameMap.frontTileIndices.map {
        case true => Some(1)
        case false => None
      }),
      emptyValue = 0
    )
    CompactGameMap(
      width = gameMap.width,
      height = gameMap.height,
      obstaclePositions = obstaclePositions,
      frontTileIndices = frontTileIndices,
      layers = layers
    )
  }
}
