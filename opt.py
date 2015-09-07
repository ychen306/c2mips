def merge_offsets(compiler):
    '''
    turn ``` 
    add addr, offset, struct
    lw value, 0(addr) ```
    into ```
    lw value, offset(struct)
    ```

    criteria of doing such transformations:
        1. `addr` is only used in loads and stores
        2. `offset` is constant
    '''
