# Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
#
# Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
#
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
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
require 'net/http'
require 'benchmark'

require 'java'
java_import  'io.sysmo.nchecks.Reply'


def check(query) # query is io.sysmo.nchecks.Query

  # Extract arguments from java Query -> Argument {asString|asInteger}
  hostname  = query.get("host").asString()
  port      = query.get("port").asInteger()
  mstimeout = query.get("ms_timeout").asInteger()

  uri    = URI("http://#{hostname}:#{port}/")
  repstr = nil
  reply  = Reply.new()

  begin
    repstr = Net::HTTP.get_response(uri)
    reply.setStatus(Reply::STATUS_OK)
    reply.setReply("HTTP Get successfull")
    reply.putPerformance("ReplyDuration", 2344)

  rescue Timeout::Error => ex
    reply.setStatus(Reply::STATUS_CRITICAL)
    reply.setReply(ex.message())

  rescue Errno::ECONNREFUSED => ex
    reply.setStatus(Reply::STATUS_CRITICAL)
    reply.setReply(ex.message())

  rescue SocketError => ex
    reply.setStatus(Reply::STATUS_ERROR)
    reply.setReply(ex.message())

  end

  return reply

end
