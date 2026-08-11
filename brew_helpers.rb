class Helpers
  @@hostname = `hostname -s`.strip
  @@docker_emulator = 'docker-desktop'

  def self.hostname
    @@hostname
  end

  def self.is_work
    self.hostname == 'walker-s'
  end

  def self.docker_emulator
    @@docker_emulator
  end
end


