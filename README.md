## Setup
========

This is basically following [this](http://confluence.jetbrains.com/display/IDEADEV/Getting+Started+with+Plugin+Development#GettingStartedwithPluginDevelopment-anchor2) guide, plus some additional steps not mentioned because everybody already knows.

1. Download IntelliJ

From [here](https://www.jetbrains.com/idea/download/).

2. Get IntelliJ sources and build IntelliJ using IntelliJ

Instructions are [here](http://www.jetbrains.org/pages/viewpage.action?pageId=983225), but watch out for the notes below:

* On modern MacOS, you will have to install JDK1.6, which you can get [here](http://support.apple.com/kb/DL1572). Once you have installed this, Java 1.6 should be in /System/Library/Java/JavaVirtualMachines. If it isn't, go to developer.apple.com/downloads, search for Java, and install the Java for OS X 2013-005 package, which ought to work.
*
* You do not have to copy lib/tools.jar anywhere. Ignore this step.
* You have to build IntelliJ, but we will never actually use the version of IntelliJ you built.

3. Configure an IntelliJ IDEA SDK

Follow the "Configuring IntelliJ IDEA SDK" from the [guide](http://www.jetbrains.org/pages/viewpage.action?pageId=983225), but watch out for the notes below.

* You must use the installation directory of the downloaded, not the built version of IntelliJ (typically, /Applications/IntelliJ IDEA/Content), which fortunately should be selected by default.
* You must use JDK 1.6 (you have named this IDEA jdk earlier) as the internal Java platform.
* Make sure you build IntelliJ first (see 2.) before doing this.

4. Open the eddy plugin

Clone git@github.com:eddysystems/eddy-plugin, and open it as a directory. It should now work.

5. To see debug output using the Logger class used in the eddy plugin, make sure you see the IDEA Log in the output, and set the debug level to "all".


### Logging

We use Amazon's DynamoDB for logging.  There is an "eddy-log" table with
primary key "install" and range key "time".  "install" is a cryptographic
random number unique to a given installation of eddy, and "time" is Greenwich
time in seconds down to milliseconds.  There is an IAM "eddy-public" user with
write-only access to eddy-log.

IMPORTANT: The credentials for eddy-public are checked into the code and
distributed along with the plugin.  This is safe because the access is
write-only.  Since the "install" key is random, a malicious user can only stomp
on their own data, which is harmless.

IAM login link: https://909287615191.signin.aws.amazon.com/console

### Release Checklist

When releasing, do at least these things: 

- Change the version number in both `.idea13/plugin.xml` and `.idea14/plugin.xml`.
- Commit and push. The build will include -dirty and/or -local for the build id if you don't do that.
- Create a tag called release-<version> and push that
- Write release notes 
- For both Idea13 and Idea14 do
  - Rebuild project
  - Make sure `resources/eddy.properties` was properly generated (the version and build number is correct)
  - Run all tests
  - Run as plugin and make sure something works
  - Run "Prepare plugin module eddy for deployment" from the Build menu
  - Run `make eddy.jar` from the command line
  - Install the resulting jar as plugin, restart, and check that it works
  - Copy the resulting .jar file to website/download/eddy-<version>-(13|14).jar
  - Upload the resulting .jar file to [plugin repository](http://plugins.jetbrains.com/plugin/7688?pr=idea)
  - Add a short version of the release notes to the plugin's feed
- Push website and pull from mt to make the version available for download immediately
- Make a forum post with the release notes
