# dd-pointerAnalyzer
# OBJ2CFA
This is our data-driven pointer analysis implemented on Doop java pointer analyzer. It contains our data-driven analysis strategies ([graphick](https://dl.acm.org/doi/10.1145/3428247) and [1callH+SL](https://dl.acm.org/doi/abs/10.1145/3498720)) that make the analysis precise and scalable. The details of the learned strategies are discussed in the papers. You can analyze java programs (.jar files) with our data-driven pointer analyzer.


#### Table of Contents

* [Getting-Started Guide](#Getting-Started-Guide)
  * [Requirements](#Requirements)
  * [Vagrant Box](#Vagrant-Box)
  * [Verifying Installation](#Verifying-Installation)
* [Artifact](#Artfact)

## Getting-Started Guide

## Requirements

- A 64-bit Ubuntu system
- A Java 8 distribution

Please set your `JAVA_HOME` environment variable to point to your Java installation directory.

### Vagrant Box
We provide a Vagrant Box to easily setup environment. The Vagrantfile is supplied to build a box with Ubuntu 18.04 LTS running on VirtualBox machine. For installation and detailed manual of it, read [Vagrant](https://vagrantup.com).


You can customize virtual machine, depending on your system spec. The following part of `Vagrantfile` can be modified for such purpose

```ruby
Vagrant.configure("2") do |config|
  # ...
  config.vm.box = "ubuntu/bionic64"
  # ...
  config.vm.provider "virtualbox" do |vb|
    vb.memory = "8192"
  # ...
  end  
  # ...
end
```

The command below creates a virtual machine and installs some dependencies (e.g., java 8). Details are described in `bootstrap.sh`. 

```sh
vagrant up
```

Now you can `ssh` the Ubuntu 18.04 VirtualBox machine and use our data-drive pointer analyzer. 

```sh
vagrant ssh
```

For running doop, Datalog engine need to be additionally installed. Please visit [this page](http://snf-705535.vm.okeanos.grnet.gr/agreement.html) and download `.deb` for Ubuntu 18.04 LTS (e.g., `pa-datalog_0.5-1bionic.deb`) and follow the instructions.

Install `.deb` package:

```sh
sudo dpkg -i pa-datalog_0.5-1bionic.deb
sudo apt-get install -f
```

Set `JAVA_HOME` to an appropriate path:
```sh
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
```
Source `lb-env-bin.sh` script:
```sh
source /opt/lb/pa-datalog/lb-env-bin.sh
```

Now, you can run our data-driven pointer analyzer. Please follow the [Verifying Installation](#Verifying-Installation).


### Verifying Installation

Verifying installation is easy. First, move to `doop/` folder. Then, you can check the installation by running the following command:

```
$ ./run.py ci -main dacapo.luindex.Main example_pgms/luindex.jar
```

You will see the results as follows:

```
running dacapo benchmark: luindex
...
Pointer analysis START
analysis time: 16.30s
Pointer analysis FINISH
...
#var points-to                        755,162
#may-fail casts                       6,670
#poly calls                           940
#reach methods                        6,670
#call edges                           33,130
```

The results say that

- It analyzed the program luindex
- The analysis 16.30s
- The results for the clients (#var points-to, #may-fail casts, #call-graph-edges, #reachable-methods, #polymorphic-calls)

### Running Doop
First, move to `doop/` foler. The command below runs Doop:

```
$ ./run.py <analysis> -main <Main> <pgm.jar>
```

<analysis> can be one of the following analyses:
```
ci, 2obj, 1callH+SL, graphick.
```
  
<pgm.jar> corresponds to the .jar file to be analzed and <Main> corresponds to the main (entry) method. For example, the following command analyzes the program luindex.jar with our data-driven analysis strategy 1callH+SL where the main method of the program is dacapo.luindex.Main:
```
$ ./run.py 1callH+SL -main dacapo.luindex.Main example_pgms/luindex.jar
```
  
  
  
 
