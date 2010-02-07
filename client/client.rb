require "socket"

class JeneverClient
  
  def initialize(host,port)
    @host, @port = host, port
  end
  
  def drink(user,number)
    send "drink #{user.downcase} #{number}" 
  end
  
  def clean(user,number,pass=nil)
    pass = get_pass if pass == nil 
    send "clean #{user} #{number} #{pass}"
  end
  
  def password
    old = get_pass("old pass: ")
    new = get_pass("new pass: ")
    send "password #{old} #{new}"
  end
  
  def table
    send "table"
  end
  
  def status(user)
    send "status #{user}"
  end
  
  private
  
  def get_pass(text="input password: ")
    print text
    `stty -echo`
    pass = $stdin.gets.chomp
    `stty echo`
    puts
    pass
  end
  
  def send(command)
    socket = TCPSocket.new(@host,@port)
    socket.puts command
    return_value = ""
    while ! socket.eof?
      return_value << socket.gets
    end
    puts return_value if return_value =~ /fail/i
  rescue Errno::ECONNREFUSED
    puts "connection refused! Is the server running?"
  ensure
    socket.close if socket
  end
  
end


