%% @author Staffan
%% @doc First draft of the chat window 
%% @copyright 2011 Peer-talk
-module(chat_frame).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(SEND, 136).
-define(ABOUT , ?wxID_ABOUT).

%% @doc Init function for chat window. 
%% @spec chat_frame:start() -> no_return().
start(User) ->
    State = make_window(User),
    loop(State),
    ok.

%% @doc Sets up the chat window. Returns {Frame, T1001,  T1002} where T1001 is the text field that displays the conversation, T1002 the input field and Frame the "super object" in the window. 
%% @spec 
make_window(User) ->
	%% Create new wx-object, new window with panel and menubar...
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, User, [{size,{430, 345}}]),
    Panel  = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),    
    Sizer1 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
	%% Create two text fields, the first is not editable.
    T1001 = wxTextCtrl:new(Panel, 1001,[{size,{100, 140}},{style, ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(T1001, false),
    T1002 = wxTextCtrl:new(Panel, 1002,[{size,{100, 60}},{style, ?wxTE_MULTILINE}]),
    B101 = wxButton:new(Panel, 101, [{label, "&Send"}]),
    B102  = wxButton:new(Panel, 997, [{label, "Send file"}]), %%
    wxSizer:add(Sizer1, T1001, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, T1002, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Sizer1,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:addSpacer(ButtonSizer, 150),
    wxSizer:add(ButtonSizer, B102,  []),
    wxSizer:addSpacer(ButtonSizer, 5),
    wxSizer:add(ButtonSizer, B101,  []),
    wxSizer:add(MainSizer, ButtonSizer, []),
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:addSpacer(MainSizer, 10), 
    wxPanel:setSizer(Panel, MainSizer),   
    %Menu bar stuff
    MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	%% ... populate it with stuff:
	wxMenu:append(Help, ?ABOUT, "About Pear Talk"),
	wxMenu:append(File, 3, "Preferences\tCtrl-P"),
	wxMenu:append(File, ?SEND, "Send\tCtrl-S"),
	wxMenuBar:append(MenuBar, File, "&File"),	
	wxMenuBar:append(MenuBar, Help, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Welcome to Pear Talk!"),
    %% Events:
    wxPanel:connect(Panel, command_button_clicked),
    wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window),
    %% Draw the window and return values:
    wxFrame:show(Frame),
    {Frame, T1001, T1002}.
		

%% @doc Writes content of input field to conversation field, then clears the input field. 
%% @spec 
write(T1001, T1002) -> 
	T =  deletenewline(wxTextCtrl:getValue(T1002)),
	wxTextCtrl:appendText(T1001,"Me: "++ T ++"\n"),
      wxTextCtrl:clear(T1002).

%% @doc Main loop, handles some events...
%% @spec 
loop(State) ->
    {Frame, T1001, T1002}  = State,  
    %io:format("--waiting in the loop--~n", []), 
    receive
    		#wx{id=?ABOUT, event=#wxCommand{}} ->
				Str = "Pear Talk is an awesmoe Peer-to-Peer Chat.",
				MD = wxMessageDialog:new(Frame,Str,
								[{style, ?wxOK bor ?wxICON_INFORMATION},
								 {caption, "About Pear Talk"}]),
				wxDialog:showModal(MD),
				wxDialog:destroy(MD),
				loop(State);
			
    		#wx{id = ?SEND, event=#wxCommand{type = command_menu_selected}} ->
    			Message = wxTextCtrl:getValue(T1002),
    			chat ! {chat_send, self(), Message},
    			write(T1001, T1002),
    			loop(State);

	    	#wx{id = 997, event=#wxCommand{type = command_button_clicked}} ->
				Obj = wxFileDialog:new(Frame,[]),
				wxFileDialog:showModal(Obj),
				File = wxFileDialog:getPath(Obj),
				chat!{chat_send_file, [File, wxFileDialog:getFilename(Obj)], self()},
				wxFileDialog:destroy(Obj),
        	        	loop(State);

    		#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
    			Message = wxTextCtrl:getValue(T1002),
			M =deletenewline(Message),
			case M of
    				"" -> 
					wxTextCtrl:clear(T1002);
				_->
    					chat!{chat_send, self(), Message},
    					write(T1001, T1002)
			end,
			loop(State);
            
            #wx{event=#wxClose{}} ->
            	io:format("~p Closing window ~n",[self()]), 
            		chat ! {close_window, self()},
         		wxWindow:destroy(Frame),
         		ok;
            
	    {message_received, Sender, Message} ->
            	wxTextCtrl:appendText(T1001,Sender++": "++Message++"\n"),
            	loop(State)
	    after 1 ->
			poll(T1001, T1002),
			loop(State)
    end.

poll(T1001, T1002) ->
	try
		T = wxTextCtrl:getValue(T1002),
		L = length(T),
		E = [lists:nth(L,T)],
		case E of
			"\n" ->
			 	Message = deletenewline(T),
	    			case (Message) of
    					"" -> 
						wxTextCtrl:clear(T1002);
					_->
    						chat!{chat_send, self(), Message},
    						write(T1001, T1002)
				end;
	    		_->
				[]
		end
	catch
		_:_ ->
			 []
	end.

deletenewline([]) ->[];
deletenewline([X|L]) ->
	case [X] of
		"\n" ->
			deletenewline(L);
		_->
			[X|deletenewline(L)]
	end.

	
	



















