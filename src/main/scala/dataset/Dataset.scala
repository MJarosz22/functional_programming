package dataset

import dataset.util.Commit.Commit
import org.joda.time.LocalDate
import org.json4s.{Formats, NoTypeHints}
import org.json4s.native.Serialization

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, SimpleTimeZone}
import scala.io.Source
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {


  /** Q23 (4p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val list = input.filter(c => c.stats.isDefined).map(c => c.stats.get.additions)
    list.sum / list.size
  }

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]) = {
    val jsFiles = input.flatMap(c => c.files).filter(f => f.filename.get.slice(f.filename.get.size-3,f.filename.get.size)==".js")
    val jsCommits = input.filter(c=> jsFiles.intersect(c.files).nonEmpty)

    val commitsByHour = jsCommits.groupBy(x => getHour(x.commit.committer.date))
    val mostActiveHour = commitsByHour.map(x => (x._1,x._2.flatMap(c => c.files).intersect(jsFiles).size)).map(x => (x._2,x._1))
      .toList.sorted.reverse.take(1).map(x => (x._2,x._1)).toList(0)
    mostActiveHour
  }

  def getHour(time: Date): Int ={
    val timeZone = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC")
    val hour = new SimpleDateFormat("HH")
    hour.setTimeZone(timeZone)
    hour.format(time).toInt
  }


  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String):(String, Int)= {
    val reposCommits = input.filter(c => c.url.split('/')(4) + '/' + c.url.split('/')(5) == repo)
    val commitsPerAuthor = reposCommits.groupBy(c => c.commit.author.name)
    val output = commitsPerAuthor.map(x => (x._1,x._2.size)).map(x => (x._2,x._1)).toList.sorted.reverse.take(1).map(x => (x._2,x._1)).toList(0)
    output
  }

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    val commitsIn2019 = input.filter(c => getYear(c.commit.committer.date) == 2019)
    val commitsByRepo = commitsIn2019.groupBy(c => (c.url.split('/')(4) + '/' + c.url.split('/')(5)))
    val numberOfCommitsByRepo = commitsByRepo.map(x => (x._1, x._2.size))
    numberOfCommitsByRepo
  }

  def getYear(time: Date): Int = {
    val year = new SimpleDateFormat("YYYY")
    val timeZone = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC")
    year.setTimeZone(timeZone)
    year.format(time).toInt
  }


  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    val files = input.flatMap(c => c.files)
    val fileTypes = files.filter(file => file.filename.get.contains('.')).groupBy(file => file.filename.get.split('.')(file.filename.get.split('.').length-1))
    //TODO: the line above is not clean and does not work for arbitrary input
    val commitsByFiletype = fileTypes.map(x => (x._1,x._2.size)).toList.map(x => (x._2,x._1)).sorted.reverse.take(5).map(x => (x._2,x._1))
    commitsByFiletype
  }



  /** Q28 (9p)
   *
   * A day has different parts:
   * Morning 5 am to 12 pm (noon)
   * Afternoon 12 pm to 5 pm.
   * Evening 5 pm to 9 pm.
   * Night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    val commitsByTime = input.groupBy(c => dayPart(getHour(c.commit.committer.date)))
    val commitsByPart = commitsByTime.map(x => (x._1, x._2.size))
    val mostProductivePart = commitsByPart.toList.map(x => (x._2,x._1)).sorted.reverse.take(1).map(x => (x._2,x._1)).toList(0)
    mostProductivePart
  }

  def dayPart(hour: Int): String = {
    if(5<=hour && 12>hour)
      return "morning"
    if(12<=hour && 17>hour)
      return "afternoon"
    if(17<=hour && 21>hour)
      return "evening"
    "night"
  }
}
