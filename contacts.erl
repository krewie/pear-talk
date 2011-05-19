%% @author Staffan
%% @doc First draft of the chat window 
%% @copyright 2011 Peer-talk
-module(contacts).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(SEND, 136).
-define(LOGOUT, 137).
-define(ABOUT , ?wxID_ABOUT).
-define(PREFERENCES, 3).
-define(FIRST_COL, 0).
-define(SECOND_COL, 1).

%% @doc 
%% @spec chat_frame:start() -> no_return().
start() ->
    State = make_window(),
    loop(State),
    ok.

%% @doc  
%% @spec 	
offline_add(AllList, INDEX, COL1, COL2, Str1, Str2, {offline}) -> 	
			wxListCtrl:insertItem(AllList, INDEX, ""),		
			wxListCtrl:setItemBackgroundColour(AllList, INDEX, {240,240,240,255}),
			wxListCtrl:setItemImage(AllList, INDEX, 0),
			wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
			wxListCtrl:setItem(AllList,INDEX, COL2, Str2);
offline_add(AllList, INDEX, COL1, COL2, Str1, Str2, {online}) ->
			wxListCtrl:insertItem(AllList, INDEX, ""),
			wxListCtrl:setItemImage(AllList, INDEX, 1),
			wxListCtrl:setItem(AllList,INDEX, COL1, Str1),
			wxListCtrl:setItem(AllList,INDEX, COL2, Str2).

fill_contact() -> 0.
%% @doc 
%% @spec 
make_window() ->
	Server = wx:new(),
	%% Create new wx-object, new window with panel, main sizer and notebook
    %Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Pear Talk", [{size,{355, 580}}]),
    Panel = wxPanel:new(Frame, []),
    MainSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Notebook = wxNotebook:new(Panel, 111, [{style, ?wxBK_DEFAULT}]),
% List with all contacts:		
    AllList = wxListCtrl:new(Notebook, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    wxListCtrl:insertColumn(AllList, ?FIRST_COL, "Contact", []),
    wxListCtrl:insertColumn(AllList, ?SECOND_COL, "Name", []),
% Pear-icons: 
    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxBitmap:new(wxImage:scale(wxImage:new("smiley.jpg", []), 16, 16))),
    wxImageList:add(IL, wxBitmap:new(wxImage:scale(wxImage:new("smiley_vit.jpg", []), 16, 16))),
% Icons associated with both list
    wxListCtrl:assignImageList(AllList, IL, ?wxIMAGE_LIST_SMALL),
% Logo:   
    BitmapSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Image = wxImage:new("icon.jpg", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image, round(wxImage:getWidth(Image)*0.34),
											   round(wxImage:getHeight(Image)*0.25),
										[{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),
% Sökfält:
	SearchSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[{label, "Friend search:"}]),
    SearchCtrl = wxTextCtrl:new(Panel, 1, [{value, "Em@il"},{style, ?wxDEFAULT},{size, {235, 20}}]),
    SearchButt = wxButton:new(Panel, 101, [{label, "&Search"}]),
    wxSizer:add(SearchSizer, SearchCtrl,[]),
    wxSizer:addSpacer(SearchSizer, 10),
    wxSizer:add(SearchSizer, SearchButt,[]),
% Sizer stuff:
    wxSizer:add(BitmapSizer, StaticBitmap, []),
    wxSizer:add(MainSizer, BitmapSizer, []),
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
	wxMenu:append(File, ?SEND, "Send\tCtrl-S"),	
	wxMenuBar:append(MenuBar, File, "&File"),	
	wxMenuBar:append(MenuBar, Help, "&Help"),
	wxMenuBar:enable(MenuBar,?SEND, false),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Welcome to Pear Talk!"),
% Events:
    wxListCtrl:connect(AllList, command_list_item_selected, []),
    wxFrame:connect(Panel, command_menu_selected),
    wxFrame:connect(Frame, close_window),
% Draw the Frame:	
    wxFrame:show(Frame),
% Return:
   {Frame, AllList}.

loop(State) ->
    {Frame, AllList}  = State,  
    io:format("--waiting in the loop--~n", []), 
    receive
    		#wx{id=?ABOUT, event=#wxCommand{}} ->
				Str = "Pear Talk is an awesmoe Peer-to-Peer Chat.",
				MD = wxMessageDialog:new(Frame,Str,
								[{style, ?wxOK bor ?wxICON_INFORMATION},
								 {caption, "About Pear Talk"}]),
				wxDialog:showModal(MD),
				wxDialog:destroy(MD),
				loop(State);
			
			#wx{id=?LOGOUT, event=#wxCommand{}} -> 
				spawn(login_frame, start, []),
				wxWindow:destroy(Frame);
			
			%#wx{id=?PREFERENCES, event=#wxCommand{}} -> 
				
        	#wx{event=#wxList{type = command_list_item_selected, itemIndex = Item}} ->
        		io:format("lala ~p \n",[Item]),
        		{Reciever, _} = lists:nth(Item+1, rul:tolist(friends)),
        		chat ! {chat_window, Reciever},
        		loop(State);  
    			
    		#wx{event=#wxList{type = command_list_delete_item, itemIndex = Item}} ->
        		io:format("lala ~p \n",[Item]),
        		wxListCtrl:deleteItem(AllList, Item),
    			loop(State);  	
    			  
            #wx{event=#wxClose{}} ->
            	io:format("~p Closing window ~n",[self()]), 
         		wxWindow:destroy(Frame),
         		ok
         	after 1000 ->
	         	wxListCtrl:deleteAllItems(AllList),
         		online_status(0, rul:tolist(friends), AllList),
         		loop(State)
            	
    end.

%% @doc 
%% @spec  
online_status(_, [], _) -> ok;
online_status(Acc, [{User, [Showed_Name, _, _]}|Rest], AllList) ->
	offline_add(AllList, Acc, ?FIRST_COL, ?SECOND_COL, Showed_Name, User, {online}),
	online_status(Acc+1, Rest, AllList);
online_status(Acc, [{User, [Showed_Name]}|Rest], AllList) -> 
	offline_add(AllList, Acc, ?FIRST_COL, ?SECOND_COL, Showed_Name,  User,{offline}),
	online_status(Acc+1, Rest, AllList).
	
	