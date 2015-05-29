require 'java'
require 'net/http'
require 'benchmark'
import  'io.sysmo.nchecks.Reply'
import  'io.sysmo.nchecks.Const'

def execute(query)
  uri = URI('http://www.sysmo.io/index.html')
  rep = nil
  delay = Benchmark.measure {
    rep = Net::HTTP.get_response(uri)
  }

  reply = Reply.new()
  reply.setReply("hello from ruby!")
  reply.setStatus(Const::STATUS_OK)
  reply.putPerformance("ReplyDuration", 2344)
  return reply
end
