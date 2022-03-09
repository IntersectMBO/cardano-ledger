{
  cbor-canonical = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1fhj51s5d9b9spw096sb0p92bgilw9hrsay383563dh913j2jn11";
      type = "gem";
    };
    version = "0.1.2";
  };
  cbor-deterministic = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1w1mg4mn1dhlxlbijxpzja8m8ggrjs0hzkzvnaazw9zm1ji6dpba";
      type = "gem";
    };
    version = "0.1.3";
  };
  cbor-diag = {
    dependencies = ["cbor-canonical" "cbor-deterministic" "cbor-packed" "json" "neatjson" "treetop"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1r84bf00h6lv05l9bb9sj27jl5kbr466qk4i18z3fimn3z62i908";
      type = "gem";
    };
    version = "0.7.6";
  };
  cbor-packed = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "046vj3i603i0skv80cynlqkh3di5419hbsfi10i69pc08wvggk6l";
      type = "gem";
    };
    version = "0.1.3";
  };
  json = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1z9grvjyfz16ag55hg522d3q4dh07hf391sf9s96npc0vfi85xkz";
      type = "gem";
    };
    version = "2.6.1";
  };
  neatjson = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0fa2v7b6433j0iqh5iq9r71v7a5xabgjvqwsbl21vcsac7vf3ncw";
      type = "gem";
    };
    version = "0.9";
  };
  polyglot = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1bqnxwyip623d8pr29rg6m8r0hdg08fpr2yb74f46rn1wgsnxmjr";
      type = "gem";
    };
    version = "0.3.5";
  };
  treetop = {
    dependencies = ["polyglot"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0697qz1akblf8r3wi0s2dsjh468hfsd57fb0mrp93z35y2ni6bhh";
      type = "gem";
    };
    version = "1.6.11";
  };
}
