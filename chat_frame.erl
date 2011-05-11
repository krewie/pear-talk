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
start(Pid) -> State = make_window(),
loop(State, Pid).

%% @doc Sets up the chat window. Returns {Frame, T1001,  T1002} where T1001 is the text field that displays the conversation, T1002 the input field and Frame the "super object" in the window. 
%% @spec 
make_window() ->
	%% Create new wx-object, new window with panel and menubar...
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Pear Talk", [{size,{430, 360}}]),
    Panel  = wxPanel:new(Frame),
	MenuBar = wxMenuBar:new(),
	Edit = wxMenu:new(),
	Help = wxMenu:new(),
	%% ... populate it with stuff:
	wxMenu:append(Help, ?ABOUT, "About Pear Talk"),
	wxMenu:append(Edit, ?SEND, "Send\tCtrl-S"),
	wxMenuBar:append(MenuBar, Edit, "&Edit"),	
	wxMenuBar:append(MenuBar, Help, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Welcome to Pear Talk!"),
	%% Create two text fields, the first is not editable.
	%% Better to do the positioning with Sizers? 
    T1001 = wxTextCtrl:new(Panel, 1001,[{pos, {2, 2}},{size, {426, 200}},
    									{style, ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(T1001, false),
    T1002 = wxTextCtrl:new(Panel, 1002,[{pos, {2, 205}},{size, {426, 70}},
    									{style, ?wxTE_MULTILINE}]),
    wxButton:new(Panel, 101, [{label, "&Send"},{pos, {335, 285}}]),
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
	case wxTextCtrl:getValue(T1002) of
    				"" -> nothing_to_write; %% bad implementation
    				_  -> T1002_val = wxTextCtrl:getValue(T1002),
            			  wxTextCtrl:appendText(T1001,T1002_val++"\n"),
            			  wxTextCtrl:clear(T1002)
    end.

%% @doc Main loop, handles some events...
%% @spec 
loop(State, Cpid) ->
    {Frame, T1001, T1002}  = State,  
    io:format("--waiting in the loop--~n", []), 
    receive
    		#wx{id=?ABOUT, event=#wxCommand{}} ->
				Str = "Pear Talk is an awesome Peer-to-Peer Chat.",
				MD = wxMessageDialog:new(Frame,Str,
								[{style, ?wxOK bor ?wxICON_INFORMATION},
								 {caption, "About Pear Talk"}]),
				wxDialog:showModal(MD),
				wxDialog:destroy(MD),
				loop(State, Cpid);
			
    		#wx{id = ?SEND, event=#wxCommand{type = command_menu_selected}} ->
    			T1002_val = wxTextCtrl:getValue(T1002),
    			write(T1001, T1002),
    			Cpid ! {send_chat, T1002_val},
    			loop(State, Cpid);
    			
    		#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
    			T1002_val = wxTextCtrl:getValue(T1002),
    			write(T1001, T1002),
    			Cpid ! {send_chat, T1002_val},
    			loop(State, Cpid);
            
            #wx{event=#wxClose{}} ->
            	io:format("~p Closing window ~n",[self()]), 
         		wxWindow:destroy(Frame),
         		ok;
         	     
         	{chat_line, Data} -> 
            	io:format("--datatata--~n", []),
            	wxTextCtrl:appendText(T1001, Data++"\n"),
            	loop(State, Cpid)
            	
    end.
