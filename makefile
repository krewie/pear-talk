
ERLC = erlc

FLAGS = -W

SERVER_DEPENDENCIES = dapi.beam detsapp.beam serv_ul.beam server.beam ul.beam

GUI_DEPENDENCIES = chat_frame.beam contacts.beam login_frame.beam

CLIENT_DEPENDENCIES = peer.beam rul.beam

compile = ${ERLC} ${FLAGS}

%.beam: %.erl
	${compile} $<

server: ${SERVER_DEPENDENCIES}

client: ${CLIENT_DEPENDENCIES} ${GUI_DEPENDENCIES}

sclient: ${CLIENT_DEPENDENCIES}
