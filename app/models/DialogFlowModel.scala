package models

import play.api.libs.json.Json

case class Intent(
  name: String,
  displayName: String
)


case class QueryResult(
  queryText: String,
  parameters: Map[String, String],
  allRequiredParamsPresent: Boolean,
  intent: Intent,
  intentDetectionConfidence: Double,
  languageCode: String
)

case class OriginalDetectIntentRequest(
  payload: Payload
)


case class Data(
  event: Event
)

case class Payload(
  data: Data
)


case class Event(
  channel: String,
  text: String,
  user: String,
)

object Event {
  implicit val format = Json.format[Event]
}


case class DialogFlowModel(
  responseId: String,
  queryResult: QueryResult,
  session: String
) {
  def getParameters: Map[String, String] = queryResult.parameters
  def isAllParameterPresent: Boolean = queryResult.allRequiredParamsPresent
  def getIntent: String = queryResult.intent.displayName
}

object Intent {
  implicit val format = Json.format[Intent]
}

object Data {
  implicit val format = Json.format[Data]
}

object Payload {
  implicit val format = Json.format[Payload]
}

object OriginalDetectIntentRequest {
  implicit val format = Json.format[OriginalDetectIntentRequest]
}

object QueryResult {
  implicit val format = Json.format[QueryResult]
}

object DialogFlowModel {
  implicit val format = Json.format[DialogFlowModel]
}
