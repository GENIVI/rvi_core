%% -*- erlang -*-
%% Test specification for testing of uart without any dongles
%%
{alias, uart_test, "."}.
{config, "uart.cfg"}.
{suites, uart_test, uart_SUITE}.
{skip_cases, uart_test, uart_SUITE, [a_to_b, modem, options], "No hardware"}.