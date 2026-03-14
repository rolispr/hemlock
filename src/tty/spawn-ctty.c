#include <unistd.h>
#include <sys/ioctl.h>
#include <signal.h>

#ifdef __APPLE__
#define TIOCSCTTY_VAL 0x20007461
#else
#define TIOCSCTTY_VAL 0x540E
#endif

pid_t spawn_with_ctty(int slave_fd, const char *path,
                      char *const argv[], char *const envp[]) {
    pid_t pid = fork();
    if (pid != 0)
        return pid;

    setsid();
    ioctl(slave_fd, TIOCSCTTY_VAL, 0);
    dup2(slave_fd, 0);
    dup2(slave_fd, 1);
    dup2(slave_fd, 2);
    if (slave_fd > 2)
        close(slave_fd);
    for (int fd = 3; fd < 1024; fd++)
        close(fd);
    signal(SIGHUP, SIG_DFL);
    signal(SIGINT, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGPIPE, SIG_DFL);
    signal(SIGCHLD, SIG_DFL);
    execve(path, argv, envp);
    _exit(127);
}
