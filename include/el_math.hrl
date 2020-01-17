
-define(wxID_TASK, 2).
-define(wxID_ANSWER, 3).
-define(wxID_BUTTON, 900).
-define(wxID_RESULT, 4).
-define(wxID_RANDOM, 5).

-define(wxID_ABOUT_MY, 799).

-define(wxID_HELP_MUL1, 701).
-define(wxID_HELP_MUL2, 702).
-define(wxID_HELP_MUL3, 703).
-define(wxID_HELP_MUL4, 704).
-define(wxID_HELP_MUL5, 705).
-define(wxID_HELP_MUL6, 706).
-define(wxID_HELP_MUL7, 707).
-define(wxID_HELP_MUL8, 708).
-define(wxID_HELP_MUL9, 709).
-define(wxID_HELP_MUL10, 700).

-define(HELP_MUL, 0).

-define(wxID_HELP_DIV1, 711).
-define(wxID_HELP_DIV2, 712).
-define(wxID_HELP_DIV3, 713).
-define(wxID_HELP_DIV4, 714).
-define(wxID_HELP_DIV5, 715).
-define(wxID_HELP_DIV6, 716).
-define(wxID_HELP_DIV7, 717).
-define(wxID_HELP_DIV8, 718).
-define(wxID_HELP_DIV9, 719).
-define(wxID_HELP_DIV10, 710).

-define(HELP_DIV, 1).

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


