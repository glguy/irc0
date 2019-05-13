#ifndef IRCCLIENT_H_
#define IRCCLIENT_H_

struct ircclient;

#define CONTINUE 0
#define SKIP     1
#define QUIT     2

void ircclient_print
  (struct ircclient *token,
   const char *msg, size_t msglen);

int ircclient_send
  (struct ircclient *token,
   const char *net, size_t netlen,
   const char *cmd, size_t cmdlen);

typedef int command_cb
  (const char *word[],
   size_t *word_lens,
   const char *word_eol[],
   size_t *word_eol_lens,
   size_t args,
   void *user_data);

typedef int message_cb
  (const char *net, size_t netlen,
   const char *pfx, size_t pfxlen,
   const char *cmd, size_t cmdlen,
   const char **args, size_t *arg_lens,
   size_t arg_len,
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

hook_id ircclient_hook_message
  (struct ircclient *token,
   const char *name, size_t namelen,
   int priority,
   message_cb *cb,
   void *userdata);

void *ircclient_unhook (struct ircclient *token, hook_id id);

typedef void strings_cb(const char **strs, const size_t *, size_t, void *);

int ircclient_query (struct ircclient *token, const char *, size_t, strings_cb*, void *);

#endif
