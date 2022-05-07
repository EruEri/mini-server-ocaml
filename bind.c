#include "caml/misc.h"
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>


CAMLprim value caml_read_response(value file_descriptor, value unit){
    int fd = Int_val(file_descriptor);
    printf("file descriptor: %d\n", fd);
    size_t read_cst = 1024;
    size_t offset = 0;
    ssize_t read = 0;

    char* buffer = malloc(read_cst);
    while (1) {
        read = recv(fd, &buffer[offset], read_cst, 0);
        if (read < read_cst) break;
        switch (read) {
        case -1:
            printf("Recv error\n");
            exit(1);
        case 0:
            printf("Disconnected");
            break;
        default:
            offset += read;
            buffer = realloc(buffer, read + offset);
            printf("Read %lu\n", read);
            
        }
    }  
    value str = caml_copy_string_of_os(buffer);
    free(buffer);
    return str;
}
