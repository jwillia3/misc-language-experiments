awk '
    /^#[^#]/ {
        check()
        name = substr($0, 3)
        mode = code = out = ""
        next
    }
    /##/ {
        i = index($0, "##")
        code = cat(code, substr($0, 1, i-1))
        out = cat(out, substr($0, i+3))
        next
    }
         { code = cat(code, $0) }
    
    END { check(); system("rm .code .check .out") }

    function cat(a, b) { return a==""? b: a "\n" b; }
    function check() {
        if (code=="") return
        print code >".code"; close(".code")
        print out >".check"; close(".check")
        print toupper(name)
        if (system("./lang-x .code | tr -d \r >.out; diff .out .check")) {
            print "____"
            print code
            code = ""; # prevent end from re-testing
            exit
        }
        code = out = name = ""
    }
'