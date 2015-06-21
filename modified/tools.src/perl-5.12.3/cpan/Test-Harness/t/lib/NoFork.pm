package NoFork;

BEGIN {
    *CORE::GLOBAL::fork = sub { die "you should not fork" };
}
use Config;
tied(%Config)->{d_fork} = 0;    # blatant lie

=begin TEST

Assuming not to much chdir:

  SPECPERLOPT='-It/lib -MNoFork' perl -Ilib bin/prove -r t

=end TEST

=cut

1;

# vim:ts=4:sw=4:et:sta
