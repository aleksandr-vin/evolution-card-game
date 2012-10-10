-ifndef(LOG_HRL).
-define(LOG_HRL, 1).

-compile([{parse_transform, lager_transform}]).

-define(debug(F, A), lager:debug(F, A)).
-define(debug(F), lager:debug(F)).

-define(info(F, A), lager:info(F, A)).
-define(info(F), lager:info(F)).

-define(error(F, A), lager:error(F, A)).
-define(error(F), lager:error(F)).

-endif.
