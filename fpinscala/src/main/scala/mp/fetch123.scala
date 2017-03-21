import scala.concurrent.Future

package object haxl {

  import java.time.Instant

  type PostId = Long
  type PostContent = List[String]
  type Html = String

  // Future => Works for now
  type Fetch[A] = Future[A]
  case class PostInfo(
      postId: PostId,
      postDate: Instant,
      postTopic: String
  )

}


package haxl.data

import haxl._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Fetcher {
/*
Our computation will be done in a monad called Fetch. The
implementation of Fetch will be given later, but for this example
all we need to know is that Fetch has instances of Monad, Functor
and Applicative, and has the following operations for fetching
data:
*/

  def getPostIds: Fetch[List[PostId]]
  def getPostInfo(postId: PostId): Fetch[PostInfo]
  def getPostContent(postId: PostId): Fetch[PostContent]
  def getPostViews(postId: PostId): Fetch[Int]

  def renderPosts(posts: List[(PostInfo, PostContent)]): Html
  def renderList(posts: List[(PostInfo, PostContent)]): Html
  def renderPage(left: Html, right: Html): Html
  def renderSidePane(left: Html, right: Html): Html
  def renderTopics(topics: List[(Html, Int)]): Html

  def getAllPostsInfo: Fetch[List[PostInfo]] =
    for {
      ids <- getPostIds
      infos <- Future.traverse(ids)(getPostInfo)
    } yield infos

  def getPostDetails(postId: PostId): Fetch[(PostInfo, PostContent)] =
    getPostInfo(postId) zip getPostContent(postId)

  val popularPosts: Fetch[Html] = {
    val posts =
      for {
        pids <- getPostIds
        views <- Future.traverse(pids)(getPostViews)
        ordered = (pids zip views)
          .sortWith((l, r) => l._2 > r._2)
          .take(5)
          .map(_._1)
        content <- Future.traverse(ordered)(getPostDetails)
      } yield content

    posts map renderList
  }

  val mainPane: Fetch[Html] = {
    val enrichedPosts =
      for {
        posts <- getAllPostsInfo
        ordered = posts
          .sortWith((l, r) => l.postDate isAfter r.postDate)
          .take(5)
        content <- Future.traverse(ordered)(post =>
          getPostContent(post.postId))
      } yield ordered zip content

    enrichedPosts map renderPosts
  }

  val topics: Fetch[Html] = {
    val tps =
      for {
        posts <- getAllPostsInfo
        topiccounts = posts
          .groupBy(_.postTopic)
          .map({ case (k, v) => (k, v.size) })
      } yield topiccounts.toList

    tps map renderTopics
  }

  val leftPane: Fetch[Html] = popularPosts.zipWith(topics)(renderSidePane)

  val blog = leftPane.zipWith(mainPane)(renderPage)

}
