%% @author Grupp 2 (Staffan Rodgren, Gabriel Tholsg�rd, Kristian Ionescu, M�rten Blomberg, G�ran Hagelin, Staffan Reinius)
%% @doc Contacts -frame. Mainframe with field for searching friends and entering new "show namn" and a list of all contacts. 
%% @copyright 2011 Peer-talk
-module(contacts).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(SEND, 136).
-define(LOGOUT, 137).
-define(ABOUT , ?wxID_ABOUT).
-define(PREFERENCES, 3).
-define(FIRST_COL, 1).
-define(SECOND_COL, 0).
-define(SEARCH, 101).
-define(CHANGE, 102).
-define(DELETE, 107).

%% @doc Start function for contacts window. 
%% @spec contacts:start() -> ok.
start() ->
    State = make_window(),
    loop(State),
    ok.

%% @doc Updates the contact list at INDEX where the user is either offline or online.   
%% @spec contacts:offline_add(wxListCtrl, Int, Int, Int, String, String, {offline}) -> ok.	
offline_add(AllList, INDEX, COL1, COL2, Str1, Str2, {offline}) ->
    try
    	wxListCtrl:insertItem(AllList, INDEX, ""),		
    	wxListCtrl:setItemBackgroundColour(AllList, INDEX, {189,189,189,255}),
   	 	wxListCtrl:setItemImage(AllList, INDEX, 0),
    	wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
    	wxListCtrl:setItem(AllList,INDEX, COL2, Str2)
    catch 
    	_:_ ->
    		io:format("coulden't add to the contact list")
    end;
offline_add(AllList, INDEX, COL1, COL2, Str1, Str2, {online}) ->
	try
    	wxListCtrl:insertItem(AllList, INDEX, ""),
    	wxListCtrl:setItemImage(AllList, INDEX, 1),
    	wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
    	wxListCtrl:setItem(AllList,INDEX, COL2, Str2)
    catch 
    	_:_ ->
    		io:format("coulden't add to the contact list")
    end.
update(AllList, INDEX, COL1, COL2, Str1, Str2, {offline}) -> 	
	try
    	wxListCtrl:setItemBackgroundColour(AllList, INDEX, {189,189,189,0}),
    	wxListCtrl:setItemImage(AllList, INDEX, 0),
    	wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
    	wxListCtrl:setItem(AllList,INDEX, COL2, Str2)
    catch 
    	_:_ ->
    		io:format("coulden't update the contact list")
    end;
update(AllList, INDEX, COL1, COL2, Str1, Str2, {online}) ->
	try
    	wxListCtrl:setItemBackgroundColour(AllList, INDEX, {255,255,255,0}),
    	wxListCtrl:setItemImage(AllList, INDEX, 1),
    	wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
   		wxListCtrl:setItem(AllList,INDEX, COL2, Str2)
    catch 
    	_:_ ->
    		io:format("coulden't update the contact list")
    end.

%% @doc Init function for Contact window.
%% @spec contacts:make_window() -> {wxFrame, wxListCtrl, wxTextCtrl, wxTextCtrl}
make_window() ->
    Server = wx:new(),
%% Create new wx-object, new window with panel, main sizer and notebook
    Frame = wxFrame:new(Server, -1, "Pear Talk", [{size,{355, 580}}]),
    Panel = wxPanel:new(Frame, []),
    MainSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Notebook = wxNotebook:new(Panel, 111, [{style, ?wxBK_DEFAULT}]),
% List with all contacts:		
    AllList = wxListCtrl:new(Notebook, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    wxListCtrl:insertColumn(AllList, ?SECOND_COL, "Contact", []),
    wxListCtrl:insertColumn(AllList, ?FIRST_COL, "Name", []),
% Pear-icons: 
    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxBitmap:new(wxImage:scale(wxImage:new("Smiley_red.png", []), 16, 16))),
    wxImageList:add(IL, wxBitmap:new(wxImage:scale(wxImage:new("Smiley.png", []), 16, 16))),
% Icons associated with both list
    wxListCtrl:assignImageList(AllList, IL, ?wxIMAGE_LIST_SMALL),
% Logo:   
    BitmapSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Image = wxImage:new("icon.png", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image, round(wxImage:getWidth(Image)*0.34),
					round(wxImage:getHeight(Image)*0.25),
					[{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),
% Change name field:
	UserSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[{label, "Change username:"}]),
    UserCtrl = wxTextCtrl:new(Panel, 1, [{value, "Name"},{style, ?wxDEFAULT},{size, {235, 20}}]),
    UserButt = wxButton:new(Panel, 102, [{label, "&Change"}]),
    wxSizer:add(UserSizer, UserCtrl,[]),
    wxSizer:addSpacer(UserSizer, 10),
    wxSizer:add(UserSizer, UserButt,[]),
% Search field:
    SearchSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[{label, "Friend search:"}]),
    SearchCtrl = wxTextCtrl:new(Panel, 1, [{value, "Em@il"},{style, ?wxDEFAULT},{size, {235, 20}}]),
    SearchButt = wxButton:new(Panel, ?SEARCH, [{label, "&Search"}]),
    wxSizer:add(SearchSizer, SearchCtrl,[]),
    wxSizer:addSpacer(SearchSizer, 10),
    wxSizer:add(SearchSizer, SearchButt,[]),
% Sizer stuff:
    wxSizer:add(BitmapSizer, StaticBitmap, []),
    wxSizer:add(MainSizer, BitmapSizer, []),
    wxSizer:add(MainSizer, UserSizer, [{flag, ?wxEXPAND}]),
    wxNotebook:addPage(Notebook, AllList, "Contacts", []),
    wxSizer:add(MainSizer, Notebook, [{proportion, 1},{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 4),
    wxSizer:add(MainSizer, SearchSizer, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),
% Menubar:
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    Help = wxMenu:new(),
% Some stuff in menu:
    wxMenu:append(Help, ?ABOUT, "About Pear Talk"),
    wxMenu:append(File, ?LOGOUT, "Log out\tCtrl-L"),	
    wxMenu:append(File, ?PREFERENCES, "Preferences\tCtrl-P"),
    wxMenu:append(File, ?DELETE, "Delete\tCtrl-D"),
    wxMenu:append(File, ?SEND, "Send\tCtrl-S"),	
    wxMenuBar:append(MenuBar, File, "&File"),	
    wxMenuBar:append(MenuBar, Help, "&Help"),
    wxMenuBar:enable(MenuBar,?SEND, false),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "Welcome to Pear Talk!"),
% Events:        
	wxListCtrl:connect(AllList, command_list_item_activated, []),
    wxListCtrl:connect(AllList, command_list_delete_item, []),
    wxListCtrl:connect(AllList, command_list_item_selected, []),
    wxFrame:connect(Panel, command_menu_selected),
    wxFrame:connect(Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
% Draw the Frame:	
    wxFrame:show(Frame),
% Return:
   {Frame, AllList, SearchCtrl, UserCtrl}.

%% @doc Receive function for Chat window. Manage the events invoked by GUI, Server or other clients.
%% @spec contacts:loop(wxFrame, wxListCtrl, wxTextCtrl, wxTextCtrl) -> ok.
loop(State) ->
    {Frame, AllList, SearchCtrl, UserCtrl}  = State,  
    receive
    {showed_name, Username} ->
    	wxTextCtrl:setValue(UserCtrl, Username),
    	loop(State);
    	
	#wx{id=?ABOUT, event=#wxCommand{}} ->
	    Str = "Pear Talk is an awesome Peer-to-Peer Chat.",
	    MD = wxMessageDialog:new(Frame,Str,
				     [{style, ?wxOK bor ?wxICON_INFORMATION},
				      {caption, "About Pear Talk"}]),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD),
	    loop(State);
	    
	#wx{id=?CHANGE, event=#wxCommand{type = command_button_clicked}} ->	
		Showed_name = wxTextCtrl:getValue(UserCtrl),
		chat ! {changename, Showed_name},
		loop(State);
					
	#wx{id=?LOGOUT, event=#wxCommand{}} -> 
	    chat ! logout,
	    wxWindow:destroy(Frame);
	
	#wx{id=?SEARCH, event=#wxCommand{type = command_button_clicked}} -> 
	    Search_string = wxTextCtrl:getValue(SearchCtrl),
	    case Search_string of
	    	[] -> ok;
	    	_ -> chat ! {gui, search, Search_string}
	    end,
	    loop(State);                 
	    
	#wx{event=#wxList{type = command_list_item_selected, itemIndex = Item}} ->
		io:format("sending frienddelete, index:~p", [Item]),
		receive
				#wx{id=?DELETE, event=#wxCommand{type = command_menu_selected}} ->
					 	Friend = wxListCtrl:getItemText(AllList, Item),
			    		chat ! {delete_friend, Friend},
	    				io:format("sending frienddelete, index:~p", [Item]),
	    				loop(State);
				_ -> 	loop(State)
		end;              
	                                            		
	#wx{event=#wxList{type = command_list_item_activated, itemIndex = Item}} ->
	    Receiver = wxListCtrl:getItemText(AllList, Item),
	    chat ! {chat_window, Receiver},
	    loop(State);  
	
	#wx{event=#wxList{type = command_list_delete_item, itemIndex = Item}} ->
	    Friend = wxListCtrl:getItemText(AllList, Item),
	    chat ! {delete_friend, Friend},
	    loop(State);  	
	
	#wx{event=#wxClose{}} ->
	    io:format("~p Closing window ~n",[self()]), 
	    chat!shut_down,
	    wxWindow:destroy(Frame),
	    ok;
	% DENNA FUNKTION KAN ANROPAS B�DE N�R MAN LOGGAR IN F�RSTA G�NGEN OCH N�R MAN L�GGER TILL NY V�N
	{client, friendlist} ->
		io:format("Entering friendlist"),
	    wxListCtrl:deleteAllItems(AllList),
	    online_status(0, rul:tolist(friends), AllList),
	    loop(State);
	% UPPDATERAR TILL MIN F�RHOPPNING R�TT PERSON P� R�TT PLATS
	{client, update, IDData} ->
	    [User, ShowedName, Status] = IDData,
	    Place = wxListCtrl:findItem(AllList, 0, User,[]),
	    update(AllList, Place, ?FIRST_COL, ?SECOND_COL, ShowedName, User, Status),
	    loop(State);
	    
	{client, search, []} ->
		Str = "No users matched",                                                                                     
	    SCD = wxSingleChoiceDialog:new(Frame, Str,"Result", []),          
	    wxSingleChoiceDialog:showModal(SCD),
	    wxSingleChoiceDialog:destroy(SCD),
	    loop(State);
	    
	{client, search, ID} ->
		Str = "Found friend.",                                                                                     
	    SCD = wxSingleChoiceDialog:new(Frame, Str,"Result", ID),          
	    wxSingleChoiceDialog:showModal(SCD),
	    Selection = wxSingleChoiceDialog:getStringSelection(SCD),
	    case Selection of
	    	[] -> ok;
	    	_ -> chat ! {gui, addfriend, Selection}
	    end,
	    wxSingleChoiceDialog:destroy(SCD),
	    loop(State)
	    
		
    end.

online_status(_, [], _) -> ok;
online_status(Acc, [{User, [Showed_Name, _, _]}|Rest], AllList) ->
    offline_add(AllList, Acc, ?FIRST_COL, ?SECOND_COL, Showed_Name, User, {online}),
    online_status(Acc+1, Rest, AllList);
online_status(Acc, [{User, [Showed_Name]}|Rest], AllList) -> 
    offline_add(AllList, Acc, ?FIRST_COL, ?SECOND_COL, Showed_Name,  User,{offline}),
    online_status(Acc+1, Rest, AllList).
