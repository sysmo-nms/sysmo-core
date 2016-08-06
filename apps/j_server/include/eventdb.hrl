%%=
%%=
% used in monitor_events, java EventDb and MailSender classes
-record(notification, {
    probe,
    check_id,
    status,
    status_code,
    time,
    return_string,
    description,
    target_display,
    target_location,
    target_contact
}).
