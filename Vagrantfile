Vagrant.configure('2') do |config|
  config.vm.box = 'bento/centos-7.4'
  config.vm.hostname = 'reflex-examples-dev'
  config.vm.network 'private_network', ip: '172.28.128.21'

  config.vm.provider 'virtualbox' do |v|
    v.memory = 4096
    v.cpus = 2
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true
  config.vm.synced_folder '.', '/src'

  config.vm.provision 'shell',
    path: 'vagrant/provision.sh',
    privileged: false,
    binary: true
end
