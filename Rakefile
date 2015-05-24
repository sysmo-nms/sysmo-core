require 'rubygems'
require 'rake'

#
# set dirs
#
ROOT       = Dir.pwd
REBAR_DIR  = File.join(ROOT, "src", "erlang")
ERLANG_DIR = File.join(ROOT, "src", "erlang", "sysmo")
JAVA_DIR   = File.join(ROOT, "src", "java")
GO_DIR     = File.join(ROOT, "src", "go")

#
# if os is windows (TODO test, maybe useless)
#
if (/cygwin|mswin|mingw|bccwin|wince|emx/ =~ RUBY_PLATFORM) != nil
  REBAR     = File.join(REBAR_DIR, "rebar.cmd")
  GRADLE    = File.join(JAVA_DIR, "gradlew.bat")
  PPING_OUT = "pping.exe"
else
  REBAR     = File.join(REBAR_DIR, "rebar")
  GRADLE    = File.join(JAVA_DIR, "gradlew")
  PPING_OUT = "pping"
end

task :default => :rel

task :build do
  cd GO_DIR;     sh "go build pping.go"
  cd ERLANG_DIR; sh "#{REBAR} -r compile"
  cd JAVA_DIR;   sh "#{GRADLE} installDist"
end

task :clean do
  cd GO_DIR;     sh "go clean pping.go"
  cd ERLANG_DIR; sh "#{REBAR} -r clean"
  cd JAVA_DIR;   sh "#{GRADLE} clean"
  cd ROOT;       sh "#{REBAR} clean"
end

task :test do
  cd ERLANG_DIR; sh "#{REBAR} -r test"
  cd JAVA_DIR;   sh "#{GRADLE} test"
end

task :check do
  cd JAVA_DIR;   sh "#{GRADLE} check"
end

task :doc do
  cd ERLANG_DIR; sh "#{REBAR} -r doc"
  cd JAVA_DIR;   sh "#{GRADLE} doc"
end

task :rel => [:build] do
  cd ROOT; sh "#{REBAR} generate"
  install_pping_command()
  puts "Release ready!"
end

task :run => [:rel] do
  cd ROOT; sh "./sysmo/bin/sysmo console"
end


def install_pping_command()
  dst = File.join(ROOT, "sysmo", "utils", PPING_OUT)
  src = File.join(GO_DIR, PPING_OUT)
  puts "Install #{dst}"
  FileUtils.copy(src,dst)
  # TODO maybe chown/chmod pping
end
