# FingerDaemon


To add user while running `fingerd`, do this

    $ telnet localhost 3000
    Trying ::1...
    telnet: connect to address ::1: Connection refused
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    
    :add "julie" "bash" "/home/julie" "Julie" "000-123-4567"
    
    User added.
    Connection closed by foreign host.
