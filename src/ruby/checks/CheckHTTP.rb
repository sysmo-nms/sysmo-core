require 'net/http'
require 'benchmark'

require 'java'
import  'io.sysmo.nchecks.Reply'

def check(query)
  uri = URI('http://www.sysmo.io/index.html')
  rep = nil
  delay = Benchmark.measure {
    rep = Net::HTTP.get_response(uri)
  }

  reply = Reply.new()
  reply.setReply("hello from ruby!")
  reply.setStatus(Reply::STATUS_OK)
  reply.putPerformance("ReplyDuration", 2344)
  return reply
end
