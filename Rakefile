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
# set wrappers
#
REBAR     = File.join(REBAR_DIR, "rebar")
GRADLE    = File.join(JAVA_DIR,  "gradlew")


#
# tasks
#
task :default => :rel

desc "Compile all"
task :build => [:java, :erlang, :pping]

desc "Compile pping"
task :pping do
  cd GO_DIR;     sh "go build pping.go"
end

desc "Compile erlang"
task :erlang do
  cd ERLANG_DIR; sh "#{REBAR} -r compile"
end

desc "Compile java"
task :java do
  cd JAVA_DIR;   sh "#{GRADLE} installDist"
end

desc "Clean all"
task :clean do
  cd GO_DIR;     sh "go clean pping.go"
  cd ERLANG_DIR; sh "#{REBAR} -r clean"
  cd JAVA_DIR;   sh "#{GRADLE} clean"
  cd ROOT;       sh "#{REBAR} clean"
end

desc "Test erlang and java apps"
task :test do
  cd ERLANG_DIR; sh "#{REBAR} -r test"
  cd JAVA_DIR;   sh "#{GRADLE} test"
end

desc "Check java apps"
task :check do
  cd JAVA_DIR;   sh "#{GRADLE} check"
end

desc "Generate documentation for java and erlang apps"
task :doc do
  cd ERLANG_DIR; sh "#{REBAR} -r doc"
  cd JAVA_DIR;   sh "#{GRADLE} doc"
end

desc "Generate a fresh release in directory ./sysmo"
task :rel => [:build] do
  cd ROOT; sh "#{REBAR} generate"
  install_pping_command()
  puts "Release ready!"
end

desc "Run the release in foreground"
task :run => [:rel] do
  cd ROOT; sh "./sysmo/bin/sysmo console"
  sh "epmd -kill"
end


#
# pping special case
#
def install_pping_command()
  dst      = File.join(ROOT, "sysmo", "utils")
  win_src  = File.join(GO_DIR, "pping.exe")
  unix_src = File.join(GO_DIR, "pping")
  if File.exist?(win_src)
    puts "Install #{win_src}"
    FileUtils.copy(win_src,dst)
  elsif File.exist?(unix_src)
    puts "Install #{unix_src}"
    FileUtils.copy(unix_src,dst)
  end
end
