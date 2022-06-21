# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/bionic64"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = "8192"
  end

  config.vm.provision "doop_deps", type: "shell",
      privileged: true, run: "once" do |dd|
    dd.path = "bootstrap.sh"
  end

end
