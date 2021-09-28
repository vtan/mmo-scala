package mmo.server.game

import mmo.common.linear.{Rect, V2}

import io.circe.Decoder

object CommonJsonDecoders {

  implicit def v2Codec[T: Decoder]: Decoder[V2[T]] =
    Decoder.decodeTuple2[T, T].map { case (x, y) => V2(x, y) }

  implicit def rectCodec[T: Decoder]: Decoder[Rect[T]] =
    Decoder.decodeTuple2[V2[T], V2[T]].map { case (xy, wh) => Rect(xy, wh) }
}
