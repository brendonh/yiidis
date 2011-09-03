-define(DBG(Term), lager:debug("~p", [Term])).
-define(ERROR(Term), lager:error("~p", [Term])).
-define(INFO(Term), lager:log(info, self(), "~p", [Term])).
-define(INFOS(Term), lager:log(info, self(), "~s", [Term])).

