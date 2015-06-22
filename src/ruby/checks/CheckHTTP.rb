require 'net/http'
require 'benchmark'

require 'java'
import  'io.sysmo.nchecks.Reply'

def check(query)
  uri    = URI('http://www.sysmo.ioo/index.html')
  repstr = nil
  reply  = Reply.new()

  begin
    delay = Benchmark.measure {
      repstr = Net::HTTP.get_response(uri)
    }
    reply.setStatus(Reply::STATUS_OK)
    reply.setReply("HTTP Get successfull")
    reply.putPerformance("ReplyDuration", 2344)

  rescue Timeout::Error
    reply.setStatus(Reply::STATUS_CRITICAL)
    reply.setReply("HTTP Get timeout")

  rescue SocketError
    reply.setStatus(Reply::STATUS_ERROR)
    reply.setReply("HTTP Get SocketError")
  end

  return reply
end
