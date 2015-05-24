require 'rubygems'
require 'rake'


#
# set dirs
#
root = Dir.pwd
erlang_dir = File.join(root, "src", "erlang")
sysmo_dir  = File.join(root, "src", "erlang", "sysmo")
java_dir   = File.join(root, "src", "java")
go_dir     = File.join(root, "src", "go")

#
# if os is windows
#
if (/cygwin|mswin|mingw|bccwin|wince|emx/ =~ RUBY_PLATFORM) != nil
  REBAR     = File.join(erlang_dir, "rebar.cmd")
  GRADLE    = File.join(java_dir, "gradlew.bat")
  GO        = "go.exe"
  PPING_OUT = "pping.exe"
  PPINT_OUT_PATH = File.join(go_dir, pping_out)
else
  REBAR      = File.join(erlang_dir, "rebar")
  GRADLE     = File.join(java_dir, "gradlew")
  GO         = "go"
  PPING_OUT  = "pping"
  PPING_OUT_PATH = File.join(go_dir, PPING_OUT)
end


task :default => :build

task :build do
  cd go_dir; sh "#{GO} build -o #{PPING_OUT} pping.go"
  cd sysmo_dir; sh "#{REBAR} -r compile"
  cd java_dir; sh "#{GRADLE} installDist"
end

task :clean do
  puts "Removing #{PPING_OUT_PATH}..."
  File.delete(PPING_OUT_PATH) if File.exist?(PPING_OUT_PATH)
  cd pwd; sh "#{REBAR} clean"
  cd sysmo_dir; sh "#{REBAR} -r clean"
  cd java_dir; sh "#{GRADLE} clean"
end

task :test do
  cd erlang_dir; sh "#{REBAR} -r test"
  cd java_dir; sh "#{GRADLE} test"
end

task :check do
  cd java_dir; sh "#{GRADLE} check"
end

task :doc do
  cd erlang_dir; sh "#{REBAR} -r doc"
  cd java_dir; sh "#{GRADLE} doc"
end

task :rel => [:build] do
  cd root; sh "#{REBAR} generate"
  install_pping_command
  puts "Release ready!"
end

task :run => [:rel] do
  sh "./sysmo/bin/sysmo console"
end



def install_pping_command()
  # TODO maybe chown/chmod pping
  puts "Install pping command"
  dst_dir = File.join(pwd, "sysmo", "utils")
  FileUtils.rm_r(dst_dir) if File.directory?(dst_dir)
  FileUtils.mkdir(dst_dir)
  dst = File.join(dst_dir, PPING_OUT)
  src = PPING_OUT_PATH
  FileUtils.copy(src,dst)

end
