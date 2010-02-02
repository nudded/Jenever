require "socket"

class TestServer
  
  def initialize(port)
    @port = port
  end
  
  def run
    server = TCPServer.open(@port)
    loop do
      s = server.accept
      puts s.gets
      s.puts "OK"
      s.close
    end
  end
  
  
end

TestServer.new(2625).run