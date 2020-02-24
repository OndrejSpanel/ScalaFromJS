package com.github.opengrabeso.scalafromjs

object GlobalConfig {
  lazy val fingerprint = { "TBD"
    /*
    // https://stackoverflow.com/a/13546822/16673
    val p = Convert.getClass.getPackage
    val name = Option(p.getImplementationTitle).getOrElse("ScalaFromJS")
    val version = Option(p.getImplementationVersion).getOrElse("Dev")
    val timestamp = {

      import java.text.SimpleDateFormat
      import java.util.TimeZone
      val date = new java.util.Date()
      val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

      // Use Madrid's time zone to format the date in
      df.setTimeZone(TimeZone.getDefault)

      df.format(date)
    }

    // Uglify Scala.js version was named like this: ScalaFromJS: 2017-11-22 17:48:10.567
    name + ": " + version + " " + timestamp
    */
  }

}
