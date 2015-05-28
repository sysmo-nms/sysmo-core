require 'java'
require 'net/http'
require 'benchmark'
import  'javaSandbox.Reply'
import  'javaSandbox.Const'

def execute(query)
  uri = URI('http://www.sysmo.io/index.html')
  rep = nil
  delay = Benchmark.measure {
    rep = Net::HTTP.get_response(uri)
  }

  puts "body  is #{rep.body}"
  puts "code  is #{rep.code}"
  puts "delay is #{delay}"

  reply = Reply.new()
  reply.setReply(query)
  reply.setStatus(Const::STATUS_OK)
  reply.putPerformance("rubyin", 2344)
  reply.putPerformance("rubyout", 2344)
  return reply
end
