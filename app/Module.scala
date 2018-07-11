import actor.TrafficActor
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

class Module extends AbstractModule with AkkaGuiceSupport {
  def configure = {
    bindActor[TrafficActor]("traffic-actor")
  }
}