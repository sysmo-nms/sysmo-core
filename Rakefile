# -*- mode: ruby -*-

# This file is part of Sysmo NMS.
#
# Sysmo NMS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Sysmo NMS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Sysmo NMS.  If not, see <http://www.gnu.org/licenses/>.

require 'rubygems'
require 'rake'
require 'builder'
require 'pathname'

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

desc "Build all"
task :build => [:java, :erl, :pping]

desc "Build pping"
task :pping do
  cd GO_DIR;     sh "go build pping.go"
end

desc "Build erlang"
task :erl do
  cd ERLANG_DIR; sh "#{REBAR} -r compile"
end

desc "Build java"
task :java do
  cd JAVA_DIR;   sh "#{GRADLE} installDist"
end

desc "Clean all"
task :clean do
  cd JAVA_DIR;   sh "#{GRADLE} clean"
  cd ERLANG_DIR; sh "#{REBAR} -r clean"
  cd GO_DIR;     sh "go clean pping.go"
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
  FileUtils.rm_rf("sysmo/java_apps")
  cd ROOT; sh "#{REBAR} generate"
  install_pping_command()
  generate_all_checks()
  puts "Release ready!"
end

desc "Run the release in foreground"
task :run do
  cd ROOT; sh "./sysmo/bin/sysmo console"
  sh "epmd -kill"
end


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


#
# generate AllChecks.xml
#
def generate_all_checks()
  puts "Building AllChecks.xml"
  FileUtils.rm_f("sysmo/docroot/nchecks/AllChecks.xml")
  checks = Dir.glob("sysmo/docroot/nchecks/Check*.xml")
  file   = File.new("sysmo/docroot/nchecks/AllChecks.xml", "w:UTF-8")
  xml    = Builder::XmlMarkup.new(:target => file, :indent => 4)
  xml.instruct! :xml, :version=>"1.0", :encoding => "UTF-8"
  xml.tag!('NChecks', {"xmlns" => "http://schemas.sysmo.io/2015/NChecks"}) do
    xml.tag!('CheckAccessTable') do
      checks.each do |c|
        puts "Add CheckUrl Value=#{c}"
        xml.tag!('CheckUrl', {"Value" => Pathname.new(c).basename})
      end
    end
  end
  file.close()
end
