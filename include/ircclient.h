#ifndef IRCCLIENT_H_
#define IRCCLIENT_H_

void ircclient_print
  (void *token,
   const char *msg, size_t msglen);

int ircclient_send
  (void *token,
   const char *net, size_t netlen,
   const char *cmd, size_t cmdlen);

#endif
