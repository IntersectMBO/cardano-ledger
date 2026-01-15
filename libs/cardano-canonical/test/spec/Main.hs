

tests = do
    descripe "BaseTypes" do
        isCanonical @"common" @Anchor
        validateType @"gov/committee/v0" @(Anchor) "anchor"
        isCanonical @"common" @EpochNo
        validateType @"gov/proposals/v0" @(EpochNo) "epoch_no"
        isCanonical @"common" @(NonNegativeInterval)
        validateType @"gov/proposals/v0" @(NonNegativeInterval) "nonnegative_interval"
        isCanonical @"common" @(ProtVer)
        validateType @"gov/pparams/v0" @(ProtVer) "protocol_version"