#ifndef IRCCLIENT_H_
#define IRCCLIENT_H_

struct ircclient;

void ircclient_print
  (struct ircclient *token,
   const char *msg, size_t msglen);

int ircclient_send
  (struct ircclient *token,
   const char *net, size_t netlen,
   const char *cmd, size_t cmdlen);

typedef void command_cb
  (const char *word[],
   size_t *word_lens,
   const char *word_eol[],
   size_t *word_eol_lens,
   size_t args,
   void *user_data);

typedef long hook_id;

hook_id ircclient_hook_command
  (struct ircclient *token,
   const char *name,
   size_t namelen,
   int priority,
   command_cb *cb,
   const char *help,
   size_t helplen,
   void *userdata);

void *ircclient_unhook
  (struct ircclient *token,
   hook_id id);


#endif
