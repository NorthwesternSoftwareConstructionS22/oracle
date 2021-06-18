#lang scribble/manual
@title[]{CI test fest setup guide}

@section{What is Github Actions?}
We will we using Github Actions CI to provide automatic feedback on assignments.

Github Actions is a service that monitors a GitHub repository; when it detects a newly pushed commit, it spins up a virtual machine and runs some scripts (that you write).
Usually, Github Actions is used to build a project and run a test suite, so that teams can have an ongoing idea of their project's status while they work on it.

We will be using it in much the same way as is typical, but the status information you will get is your score for each assignment.
On every push, Github Actions will run your submission against the tests submitted by every team and report the results.

@section{Setup guide}
@subsection{Obtain a GitHub education account}
If you don't already have one, go to @hyperlink["https://education.github.com/"]{this GitHub page} and click "Get benefits".

@subsection{Configure what Github Actions does on each push}
Github Actions is configured with a special configuration file containing commands to execute on every new push.

In the top level of your repository (the one containing the @tt{.git} folder), create a directory named @tt{.github/workflows} and inside it a file named @tt{test.yml} with the following contents.
@verbatim|{
on:
  - push

jobs:
  test:
    runs-on: ubuntu-20.04
    env:
      MAJOR: 3
      MINOR: 1
      TEAM: teamX
      LOGLEVEL: error # Valid options are: error | info | debug
    steps:
      - name: Checkout
        uses: actions/checkout@main
      - name: Install Racket
        uses: Bogdanp/setup-racket@v0.11
        with:
          architecture: 'x64'
          distribution: 'full'
          version: '8.0'
      - name: Install oracle
        run: raco pkg install -D --auto https://github.com/NorthwesternSoftwareConstructionS21/oracle.git
      - name: Run tests
        run: racket -O "$LOGLEVEL"@fest -W none -l software-construction-admin -- -M $MAJOR -m $MINOR -n "$TEAM"
}|

The first section of this file specifies when the service should do something: in this case, whenever you push to github.
Next, the file specifies the jobs to perform on every push: in this case, you just need one job that will test your submission against everyone's tests (the job is called "test").
The job will run on an Ubuntu 20.04 virtual machine, and it has four environment variables which you need to fill in to specify which assignment to test, which team you are, and what log level you want from the testing system.
When you create this file you need to change @tt{TEAM} to be your team name, and you won't need to change it again.
The @tt{MAJOR} and @tt{MINOR} variables together specify the assignment to test, so you will need to change them for every assignment.
Finally the @tt{LOGLEVEL} variable affects how much output the testing system produces: @tt{error} means to just report test failures, while @tt{info} will report a little more information about how the tests are being run.
In the example, this is assignment "3.1" and a team named "teamX".

@bold{Important}: As you start working on new assignments, you must change the assignment numbers here to specify the new assignment to test.

Note that you only want a single one of these @tt{env} blocks.

You do not need to read the rest of the configuration;
that said, if you're curious, the next section specifies what Github Actions should do on the virtual machine.
Each bullet is an action (hence the name of the service), and they all boil down to executing commands on the virtual machine.
The example actions do the following:
@itemlist[#:style 'ordered
@item{Checkout the @tt{main} branch of the repo
}
@item{Install Racket and the oracle
}
@item{Run the testing system to 1) build the submission under @tt{Deliverables} using @tt{make}, and 2) test the submission, assuming @tt{make} created an executable called @tt{run}
}
]

@bold{Important}: The executable built from your submission @italic{must} be called @tt{run}, and it must be have @hyperlink["https://www.linux.com/tutorials/understanding-linux-file-permissions/"]{execute permissions} (e.g. by using @tt{chmod u+x run} as part of building).

@bold{To make Github Actions test your assignment with this configuration, all that you need to do is provide a makefile in the right directory under Deliverables to build your executable} (which must be named @tt{run}).

@bold{You may not modify the yml file because the automated grading will use a stock one (exactly as above, except with an appropriate assignment number).} But, that said, a temporary modification is okay.
See @secref["sec:debugging"].

@subsection{Creating a makefile that builds your submission}
@subsubsection{Makefiles}
We will build your submission using a @hyperlink["http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/"]{makefile}.
A makefile is just a file that describes how to build your project.

Suppose that you have a project in hello-world.c that you want to compile.
A makefile for that project might look like the one below.

The first line declares a rule with a name and then a colon, followed by a list of files that must exist to run the rule.
That list can be empty.
The following lines are shell commands to execute to build your project.

@verbatim|{
# This is a file named "makefile"
rule-name: hello-world.c
	echo "Each line is a shell command"
	echo "The commands must start with a TAB character (NOT SPACES)"
	gcc -Wall hello-world.c -o run

another-rule-with-no-dependencies:
	echo "I have no dependencies"
}|

In the directory with this file, running
@verbatim|{
make}|

will execute the @bold{first rule in the file}.

You can also specify which rule to run like so
@verbatim|{
make another-rule-with-no-dependencies}|

@subsubsection{Submission makefiles}
You must provide a makefile to build each of your assignments.
The makefile you submit must be able to build your submission in a fresh environment (on Github Actions).
Thus, @bold{the makefile needs to download and install any dependencies that it needs}.
It's also important that it only installs things not already installed, so that it won't break if, for example, we build two assignments in a row.

The Github Actions virtual machines run Ubuntu, so the Ubuntu software repositories are available to install packages easily.
Otherwise, dependencies can be installed from source or from binary distributions.

For example, if you are using python, your makefile can do something like this to set up the environment.
@verbatim|{
install-python:
	sudo apt-get update
	-sudo apt-get install -y python3 # dash allows this line to fail without stopping the build, e.g. if Python is already installed
}|

As an example of installing software from a distribution, suppose that the version of java available in the Ubuntu repositories isn't what you want.
Instead, you can obtain the version you want manually from @hyperlink["https://jdk.java.net/"]{openJDK}. With JDK 13, for example, that would look like this.
@verbatim|{
download-java:
	wget "https://download.java.net/java/GA/jdk13/5b8a42f3905b406298b72d750b6919f6/33/GPL/openjdk-13_linux-x64_bin.tar.gz"
	tar -xf openjdk-13_linux-x64_bin.tar.gz
	echo "java binaries are now in ./jdk-13/bin/"
}|

@bold{You should not use any special Github Actions features} to install dependencies, because then we won't be able to build your assignment to grade it.
@bold{Important: Grading will happen in the same environment as the Github Actions jobs}, so your job must be able to build the assignment.
Specifically, we will be using a configuration file nearly identical to the example given earlier for grading.

@subsubsection{The built executable should be able to just be run}
We assume that your makefile builds an executable called @tt{run}, and we should be able to execute it as-is (e.g. with @tt{./run}).
It might be easiest to make the @tt{run} executable be a script that runs the submission.

For example, if you are using Python then you might put a file named @tt{run} that looks like this:
@verbatim|{
#!/bin/bash

# This is the file "run": it runs the actual submission

python my-submission.py some arguments
}|

Make sure that the file has @hyperlink["https://www.linux.com/tutorials/understanding-linux-file-permissions/"]{execute permission}, for instance by running
@verbatim|{
chmod u+x run}|

@subsection{Commit the Github Actions configuration and push}
Go to the github page for your repo and click on the tab named "Actions".
You will see a list of "All workflows", at the top of which will be an in-progress job.
Clock on it and click on the box that says "test" to watch the job's progress.

@subsection{Find your test results summary in the job log}
The job will fail if you do not have a perfect score on the assignment specified in the @tt{.yml} configuration file.
This can happen in two ways:
@itemlist[#:style #f
@item{Fewer than expected of the submitted tests were found to be valid
}
@item{The submission does not pass all of your peer's tests
}
]

If the job fails, first look at the end of the log for a summary of what went wrong.
Then, scan the log for messages that look like this
@verbatim|{
fest: <message here>}|

These will indicate what went wrong and how.



@subsection{Queue Times}
GitHub has limits on the jobs that we all can run; they allow only six jobs at once across
the whole organization (i.e., six jobs at once for all students in Software
Construction), and we have a finite total amount of time that our jobs can use each month.
If everyone is trying to run jobs at once, queue times may become long; we recommend trying to avoid
using Actions close to the homework deadlines to avoid long
waits.

If you do experience long queue times or jobs not starting at all, or you don't want to deal with the possibility of it happening, you can run actions outside of the organization instead.
As a student with a GitHub Education account, you get a large amount of time each month for free that you can use for Actions jobs.
These are completely independent of the course organization, so you will not experience any queue times, and the amount of free time available is generous.

To run actions using your own account's free resources instead of the organization's, you can set up another git repository on
GitHub that is outside of the organization, via your own
GitHub userid. Specifically, create a @bold{private} repository from your own
GitHub webpage and then, if you named the repository @tt{sc},
you can add it as a remote and push to it like this:
@verbatim{
 # add a remote
 git remote add me https://github.com/@italic{«yourid»}/sc.git
 # push to your personal remote
 git push me main
}

Of course, only pushes to the repository in the
course organization count towards your grade.



@subsection[#:tag "sec:debugging"]{Debugging GitHub Actions CI}
If you are having trouble with GitHub Actions, here are a few things you can try:
@itemlist[#:style 'ordered
@item{Modify the @tt{.yml} configuration to @bold{temporarily} add commands to look around
@itemlist[#:style #f
@item{E.g. if GitHub Actions can't find the assignment directory, try adding a line like this under the @tt{steps:} line.
@verbatim|{
- run: ls -l Deliverables Deliverables/2 Deliverables/2/2.1}|
}
@item{You can also write a shell script to try things out on the CI and run it in the same way.
}
@item{Note that @bold{after resolving the issue you should go back to the original config} like above to ensure that your assignment can build when we grade it
}
]
}
@item{Increase the logging level in your configuration to @tt{debug} if the problem seems to happen in the testing system. This will give you more information about what might have gone wrong.
}
@item{Modify your makefile to @bold{temporarily} add debugging commands.
}
]

Once you resolve the problem, remember to undo any changes you made to the @tt{.yml} configuration, otherwise you might get out of sync with the configuration we will use for grading -- this could allow tests to pass on your jobs but fail when we grade the same submission.
