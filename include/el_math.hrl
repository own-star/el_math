
-define(wxID_TASK, 2).
-define(wxID_ANSWER, 3).
-define(wxID_BUTTON, 900).
-define(wxID_RESULT, 4).
-define(wxID_RANDOM, 5).

-record(state, {command = 302,
                action = mul,
                a = 2,
                b = 2,
                right = 0,
                wrong = 0,
                frame,
                timer_set = disable,
                timer = disable,
                rand_list = [],
                random = true
               }).


