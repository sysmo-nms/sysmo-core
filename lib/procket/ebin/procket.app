{application,procket,
    [
        {description,"Low level socket operations"},
        {vsn,"0.03"},
        {modules,[bpf,packet,procket,procket_ioctl,procket_mktmp]},
        {applications, [kernel,stdlib]},
        {registered, [procket]}
    ]
}.
