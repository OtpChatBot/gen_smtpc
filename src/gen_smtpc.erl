-module(gen_smtpc).

-behaviour(gen_server).
 
-export([start_link/5]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Api
-export([send/5]).
 
-record(state, {}).
 
start_link({From, FromPassword}, To, Subject, Body, Options) ->
    gen_server:start_link(?MODULE, [{From, FromPassword}, To, Subject, Body, Options], []).
 
init([{From, FromPassword}, To, Subject, Body, Options]) ->
    % start to send mail
    gen_server:cast(self(), {send_mail, From, FromPassword, To, Subject, Body, Options}),
    % return
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Send mail
%% @end
handle_cast({send_mail, From, FromPassword, To, Subject, Body, Options}, State) ->
    % Check ssl
    Ssl = case lists:keyfind(use_ssl, 1, Options) of
            {use_ssl, true} ->
                ssl;
            _ ->
                gen_tcp
            end,
    % Get host
    Host = case lists:keyfind(host, 1, Options) of
        {host, ConnectionHost} ->
            ConnectionHost;
        _ ->
            io:format("ERROR: Options must contain {host, Host} ~n"),
            % stop current process
            {stop, normal, State}
        end,
    % Get port
    Port = case lists:keyfind(port, 1, Options) of
        {port, ConnectionPort} ->
            ConnectionPort;
        _ ->
            % return default smtp port
            25
        end,
    % send message
    send_mail(From, FromPassword, To, Subject, Body, Ssl, Host, Port, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ssl_closed, _Reason}, State) ->
    % try reconnect
    {stop, normal, State};

handle_info({ssl_error, _Socket, _Reason}, State) ->
    % try reconnect
    {stop, normal, State};

handle_info({tcp_closed, _Reason}, State) ->
    % try reconnect
    {stop, normal, State};

handle_info({tcp_error, _Socket, _Reason}, State) ->
    % try reconnect
    {stop, normal, State};

%% @doc Incoming message
handle_info({_, _Socket, _Data}, State) ->
    % return
    {noreply, State#state{}};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions

%% @doc Send mail message via smtp
%% @end
send({From, FromPassword}, To, Subject, Body, Options) ->
    gen_smtpc_sup:start_mail_client({From, FromPassword}, To, Subject, Body, Options),
    ok.

send_mail(From, FromPassword, To, Subject, Body, Ssl, Host, Port, State) ->
    % Connection options
    Opts = case Ssl of
        ssl -> 
            [{delay_send, false}, {verify, 0}, {nodelay, true}];
        gen_tcp -> 
            [{delay_send, false}, {nodelay, true}]
    end,
        
    % Connect to smtp server
    case Ssl:connect(Host, Port, Opts) of
        {ok, Socket} ->
            Ssl:send(Socket, ["HELO ", proplists:get_value(hostname, Opts, guess_FQDN()), "\r\n"]),
            Ssl:send(Socket, "AUTH LOGIN\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, binary_to_list(base64:encode(From)) ++ "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, binary_to_list(base64:encode(FromPassword)) ++ "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "MAIL FROM: <" ++ From ++ ">\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "RCPT TO: <" ++ To ++ ">\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "DATA\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "From: <" ++ From ++ ">\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "To: <" ++ To ++ ">\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "Subject: " ++ Subject ++ "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, Body ++ "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, ".\r\n"),
            timer:sleep(2000),
            Ssl:send(Socket, "QUIT\r\n"),
            timer:sleep(2000),
            ssl:close(Socket),
            {noreply, State};
        {error, Error} ->
            io:format("ERROR: ~p~n", [Error]),
            {noreply, State}
    end.
    
guess_FQDN() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Hostent} = inet:gethostbyname(Hostname),
    {hostent, FQDN, _Aliases, inet, _, _Addresses} = Hostent,
    FQDN.