Config { position = Top
       , font     = "xft:Source Code Pro:size=11:bold:antialias=true"
       , fgColor = "#ffffff"
       , bgColor = "#000000"
       , alpha = 50  -- 0 transparent, 255 opaque
       , sepChar = "%"
       , alignSep = "}{"
       , commands = [ Run StdinReader
                    , Run Date "%A %_d %B %_I:%M" "date" 300
                    ]
       , template = " %StdinReader% } %date% {"
       }
