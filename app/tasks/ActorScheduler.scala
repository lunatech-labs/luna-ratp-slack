package tasks

import actors.TrafficActor
import akka.actor.{ActorRef, ActorSystem}
import javax.inject.{Inject, Named}
import play.api.inject.{SimpleModule, _}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ActorScheduler @Inject()(actorSystem: ActorSystem, @Named("traffic-actor") trafficActor: ActorRef)(implicit executionContext: ExecutionContext) {

  actorSystem.scheduler.schedule(
    initialDelay = 0.microseconds,
    interval = 30.seconds,
    receiver = trafficActor,
    message = TrafficActor.RER()
  )

  actorSystem.scheduler.schedule(
    initialDelay = 0.microseconds,
    interval = 30.seconds,
    receiver = trafficActor,
    message = TrafficActor.METRO()
  )
}

class ActorSchedulerModule extends SimpleModule(bind[ActorScheduler].toSelf.eagerly())