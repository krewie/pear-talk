
ERLC = erlc

FLAGS = -W

SERVER_DEPENDENCIES = dapi.beam serv_ul.beam server.beam upnp.beam

GUI_DEPENDENCIES = chat_frame.beam contacts.beam login_frame.beam reg_frame.beam dialog.beam

CLIENT_DEPENDENCIES = peer.beam rul.beam smtp.beam upnp.beam

compile = ${ERLC} ${FLAGS}

%.beam: %.erl
	${compile} $<

server: ${SERVER_DEPENDENCIES}

client: ${CLIENT_DEPENDENCIES} ${GUI_DEPENDENCIES}

sclient: ${CLIENT_DEPENDENCIES}
