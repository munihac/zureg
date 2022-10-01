import qualified Zureg.Hackathon
import qualified Zureg.Main.BadgesPdf

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.BadgesPdf.main
