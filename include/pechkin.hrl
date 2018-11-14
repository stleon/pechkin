-ifndef(PECHKIN_HRL).

-define(PECHKIN_HRL, true).
-define(TELEGRAM_MESSAGE, telegram_message).
-define(TELEGRAM_WORKERS, telegram_workers).
-define(TELEGAM_WORKER(I), {telegram_worker, I}).


-record(?TELEGRAM_MESSAGE, {
          apikey  :: bitstring(),
          channel :: bitstring(),
          text    :: bitstring()
         }).

-endif.
