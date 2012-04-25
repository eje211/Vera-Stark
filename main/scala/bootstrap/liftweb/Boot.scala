package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._
import java.sql.{Connection, DriverManager}
import java.lang.Integer

// import our models
import edu.cmu.etc.verastark.model._
import edu.cmu.etc.verastark.snippet._
import edu.cmu.etc.verastark.lib._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    // }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    // Schemifier.schemify(true, Schemifier.infoF _, User, Autobiography)
    Schemifier.schemify(true, Schemifier.infoF _, User, Autobiography, Item, Artifact, Comment, Annotation)

    // where to search snippet
    // LiftRules.addToPackages("code")
    LiftRules.addToPackages("edu.cmu.etc.verastark")

    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Index") / "index" >> Hidden, // >>  User.AddUserMenusAfter, // the simple way to declare a menu
      Menu.i("Vera") / "vera" >> Hidden >> LocGroup("left"), // >>  User.AddUserMenusAfter, // the simple way to declare a menu

      ArtifactPageMenu.menu >> Hidden,
      Menu(Loc("ArtifactStaticLink", Link("artifact" :: "index" :: Nil, true, "/artifact/index"), "Artifact", LocGroup("left"), Hidden)),

      AutobiographyPageMenu.menu >> Hidden,
      Menu(Loc("AutobiographyStaticLink", Link("autobiography" :: "index" ::  Nil, true, "/autobiography/index"), "Autobiography", LocGroup("left"), Hidden)),

      Menu(Loc("Add to the Legacy", Link("artifact" :: "new" ::  Nil, true, "/artifact/new"), "Add to the Legacy", LocGroup("left"), Hidden)),

      // more complex because this menu allows anything in the
      // /static path to be visible
      // Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content", LocGroup("left"), Hidden)),
      Menu.i("Herb") / "about" >> Hidden >> LocGroup("left"),
      
      Menu.i("Backstage") / "backstage" >> Hidden >> LocGroup("left"),

      Menu.i("Moderaation") / "moderate" >> If(() =>(User.currentUser.map(u => u.superUser.is || u.editor.is) openOr false), "Only editors can moderate."),
      Menu.param[User]("User", "User", (s: String) => 
        User.find(By(User.id, try {Integer.parseInt(s)} catch {case (e: NumberFormatException) => 0} ))
        , _.toString) / "user" >> Hidden
    )

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))    
    
    LiftRules.maxMimeFileSize = 40000000L
    LiftRules.maxMimeSize = 40000000L
    LiftRules.dispatch.append(UploadManager)

    // // Use jQuery 1.4
    // LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // OpenID rules
    LiftRules.dispatch.append(DefaultOpenIDVendor.dispatchPF)
    LiftRules.snippets.append(DefaultOpenIDVendor.snippetPF)

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
}

object DBVendor extends ConnectionManager {
 def newConnection(name: ConnectionIdentifier): Box[Connection] = {
   try {
     Class.forName("com.mysql.jdbc.Driver")
     val dm = DriverManager.getConnection("jdbc:mysql://localhost/verastark?user=verastark&password=beyondthestage")
     Full(dm)
   } catch {
     case e : Exception => e.printStackTrace; Empty
   }
 }
 def releaseConnection(conn: Connection) {conn.close}
}
