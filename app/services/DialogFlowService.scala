package services

import javax.inject.Inject
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient

class DialogFlowService @Inject()(ws: WSClient, config: Configuration) {

  def forwardBody(json: Option[JsValue]) = {
    json.map(j =>
      ws
        .url(config.get[String]("dialogflow.event.url"))
        .post(j)
    )
  }

}
