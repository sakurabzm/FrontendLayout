package de.tkip.sbpm.frontend.Data

object Expense {

    import io.circe.{Encoder, Decoder}
    import io.circe.generic.semiauto.{deriveEncoder, deriveDecoder}

    implicit val TransitionDataDecoder: Decoder[TransitionData] = deriveDecoder[TransitionData]
    implicit val TransitionDataEncoder: Encoder[TransitionData] = deriveEncoder[TransitionData]
    implicit val StateJsonDataDecoder: Decoder[StateJsonData] = deriveDecoder[StateJsonData]
    implicit val StateJsonDataEncoder: Encoder[StateJsonData] = deriveEncoder[StateJsonData]
    implicit val SubjectContentDecoder: Decoder[SubjectContent] = deriveDecoder[SubjectContent]
    implicit val SubjectContentEncoder: Encoder[SubjectContent] = deriveEncoder[SubjectContent]
}