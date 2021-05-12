module City.Education exposing (def, id)

import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)


id : String
id =
    "education-high-school"


def : Svg msg
def =
    highSchool


highSchool : Svg msg
highSchool =
    svg [ Attr.id id, viewBox "0 0 61.625 58.846" ]
        [ path [ d "M15.622 21.603l15.447-9.734-15.44-10.09L.147 11.513z", fill "#d9a371" ]
            []
        , path [ d "M15.633 39.024l-.046-17.42 15.482-9.735v17.464z", fill "#d9a371" ]
            []
        , path [ d "M.147 11.46v17.465l15.475 10.088V21.549z", fill "#efe0a9" ]
            []
        , path [ d "M23.444 11.377L7.928 1.147.147 11.514l15.475 10.089z", fill "#c13b33" ]
            []
        , path [ d "M7.928 1.147l7.742.512 15.4 10.23-7.626-.512z", fill "#8d1c25" ]
            []
        , path [ d "M15.622 39.012V37.66L0 27.485v1.353z", fill "#d9a371" ]
            []
        , path [ d "M15.622 21.603l.004.374L23.58 11.69l7.49.42v-.22l-7.626-.513z", fill "#690d15" ]
            []
        , path [ d "M15.626 21.976L.146 11.87v-.355l15.476 10.089z", fill "#a31d31" ]
            []
        , path [ d "M15.626 37.66l15.443-9.665v1.338l-15.443 9.69z", fill "#a97d59" ]
            []
        , path [ d "M2.209 19.288v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M2.508 15.955l-.3-.183v3.578l.3-.186z", fill "#d9a371" ]
            []
        , path [ d "M2.209 19.35l2.984 1.846v-.36l-2.685-1.672z", fill "#a97d59" ]
            []
        , path [ d "M2.508 16.573l.98 3.2.68.424-1.203-3.962-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M3.815 16.755l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M3.981 16.857l.899 3.784.109.068-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M6.433 22v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M6.731 18.667l-.298-.183v3.579l.298-.187z", fill "#d9a371" ]
            []
        , path [ d "M6.433 22.063l2.984 1.845v-.36L6.73 21.875z", fill "#a97d59" ]
            []
        , path [ d "M6.731 19.285l.98 3.2.68.424-1.203-3.962-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M8.038 19.468l-.18-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M8.204 19.569l.899 3.784.109.068-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M10.656 24.798v-3.517l2.984 1.828v3.598z", fill "#41c3db" ]
            []
        , path [ d "M10.955 21.465l-.3-.184v3.579l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M10.656 24.86l2.984 1.846v-.36l-2.685-1.673z", fill "#a97d59" ]
            []
        , path [ d "M10.955 22.082l.98 3.202.68.423-1.203-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M12.262 22.264l-.18-.11v3.221l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M12.427 22.366l.9 3.784.108.068-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M2.209 26.042v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M2.508 22.709l-.3-.184v3.58l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M2.209 26.104l2.984 1.846v-.36l-2.685-1.672z", fill "#a97d59" ]
            []
        , path [ d "M2.508 23.326l.98 3.202.68.423-1.203-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M3.815 23.509l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M3.981 23.61l.899 3.785.109.067-.878-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M6.433 28.754v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M6.731 25.422l-.298-.184v3.578l.298-.186z", fill "#d9a371" ]
            []
        , path [ d "M6.433 28.816l2.984 1.846v-.36L6.73 28.63z", fill "#a97d59" ]
            []
        , path [ d "M6.731 26.039l.98 3.201.68.424L7.188 25.7l-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M8.038 26.222l-.18-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M8.204 26.323l.899 3.784.109.067-.878-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M10.656 31.552v-3.516l2.984 1.826v3.598z", fill "#41c3db" ]
            []
        , path [ d "M10.955 28.219l-.3-.183v3.578l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M10.656 31.614l2.984 1.846v-.36l-2.685-1.673z", fill "#a97d59" ]
            []
        , path [ d "M10.955 28.836l.98 3.201.68.423-1.203-3.962-.457-.279z", fill "#80cfdf" ]
            []
        , path [ d "M12.262 29.019l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M12.427 29.12l.9 3.784.108.069-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M28.769 24.755l-15.393-9.679L32.638 2.523l15.484 9.737z", fill "#efe0a9" ]
            []
        , path [ d "M28.804 49.937v-25.18L13.32 15.02v25.231z", fill "#efe0a9" ]
            []
        , path [ d "M48.122 12.26v22.936L28.769 49.977V24.701z", fill "#d9a371" ]
            []
        , path [ d "M20.946 12.637L40.34 0l7.782 12.26-19.353 12.495z", fill "#c13b33" ]
            []
        , path [ d "M40.34.022L32.6 2.427 13.284 15.041l7.661-2.382z", fill "#c13b33" ]
            []
        , path [ d "M28.769 49.977l.035-1.341L13.321 38.9v1.352z", fill "#d9a371" ]
            []
        , path [ d "M28.77 24.755l-.003.375-7.94-12.16-7.542 2.293v-.221l7.661-2.465z", fill "#a31d31" ]
            []
        , path [ d "M28.769 24.755L48.122 12.26v.397L28.767 25.13z", fill "#690d15" ]
            []
        , path [ d "M28.769 48.613l1.934-1.21-.02 1.339-1.914 1.235z", fill "#a97d59" ]
            []
        , path [ d "M17.108 42.608v-9.034l6.705 4.17v9.123z", fill "#d9a371" ]
            []
        , path [ d "M17.826 42.246v-8.23l5.987 3.727v8.312z", fill "#355b88" ]
            []
        , path [ d "M17.108 42.608l.743-.345 5.962 3.792v.812z", fill "#efe0a9" ]
            []
        , path [ d "M18.398 38.634v-3.517l5.042 3.1v3.598z", fill "#41c3db" ]
            []
        , path [ d "M21.035 41.814h-.232V36.14h.232z", fill "#355b88" ]
            []
        , path [ d "M46.179 41.425l15.446-9.733L46.186 21.6l-15.482 9.737z", fill "#d9a371" ]
            []
        , path [ d "M46.19 58.846l-.046-17.419 15.481-9.735v17.464z", fill "#d9a371" ]
            []
        , path [ d "M30.703 31.282v17.465L46.18 58.835V41.37z", fill "#efe0a9" ]
            []
        , path [ d "M54 31.2L38.486 20.97l-7.781 10.367L46.18 41.425z", fill "#c13b33" ]
            []
        , path [ d "M38.485 20.97l7.742.512 15.398 10.23L54 31.2z", fill "#8d1c25" ]
            []
        , path [ d "M46.179 58.835v-1.353L30.557 47.307v1.353z", fill "#d9a371" ]
            []
        , path [ d "M46.179 41.425l.004.374 7.954-10.286 7.488.42v-.221l-7.624-.511z", fill "#690d15" ]
            []
        , path [ d "M46.183 41.8l-15.48-10.108v-.355L46.18 41.425z", fill "#a31d31" ]
            []
        , path [ d "M46.183 57.48l15.442-9.663v1.339l-15.442 9.69z", fill "#a97d59" ]
            []
        , path [ d "M32.766 39.111v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M33.065 35.777l-.3-.183v3.579l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M32.766 39.173l2.984 1.846v-.36l-2.685-1.673z", fill "#a97d59" ]
            []
        , path [ d "M33.065 36.396l.98 3.2.68.424-1.203-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M34.372 36.577l-.18-.11v3.221l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M34.538 36.68l.9 3.783.108.068-.879-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M36.99 41.823v-3.516l2.984 1.827v3.596z", fill "#41c3db" ]
            []
        , path [ d "M37.288 38.49l-.299-.183v3.578l.299-.186z", fill "#d9a371" ]
            []
        , path [ d "M36.99 41.885l2.984 1.845v-.36l-2.686-1.67z", fill "#a97d59" ]
            []
        , path [ d "M37.288 39.108l.98 3.2.68.424-1.203-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M38.595 39.29l-.18-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M38.76 39.391l.9 3.784.109.068-.879-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M41.212 44.62v-3.516l2.985 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M41.511 41.288l-.299-.184v3.578l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M41.212 44.682l2.985 1.846v-.36l-2.685-1.673z", fill "#a97d59" ]
            []
        , path [ d "M41.511 41.905l.98 3.2.68.424-1.203-3.963-.457-.278z", fill "#80cfdf" ]
            []
        , path [ d "M42.819 42.087l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M42.984 42.189l.9 3.784.108.067-.879-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M32.766 45.864v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M33.065 42.531l-.3-.183v3.578l.3-.185z", fill "#d9a371" ]
            []
        , path [ d "M32.766 45.926l2.984 1.846v-.36l-2.685-1.671z", fill "#a97d59" ]
            []
        , path [ d "M33.065 43.149l.98 3.201.68.423-1.203-3.962-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M34.372 43.332l-.18-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M34.538 43.433l.9 3.784.108.067-.879-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M36.99 48.577V45.06l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M37.288 45.243l-.299-.183v3.578l.299-.185z", fill "#d9a371" ]
            []
        , path [ d "M36.99 48.638l2.984 1.846v-.36l-2.686-1.671z", fill "#a97d59" ]
            []
        , path [ d "M37.288 45.861l.98 3.201.68.423-1.203-3.962-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M38.595 46.044l-.18-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M38.76 46.145l.9 3.784.109.067-.879-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M41.212 51.374v-3.516l2.985 1.826v3.598z", fill "#41c3db" ]
            []
        , path [ d "M41.511 48.041l-.299-.183v3.579l.3-.188z", fill "#d9a371" ]
            []
        , path [ d "M41.212 51.437l2.985 1.845v-.36l-2.685-1.673z", fill "#a97d59" ]
            []
        , path [ d "M41.511 48.658l.98 3.201.68.423-1.203-3.962-.457-.279z", fill "#80cfdf" ]
            []
        , path [ d "M42.819 48.841l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M42.984 48.942l.9 3.785.108.068-.879-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M14.909 23.196V19.68l2.983 1.827v3.598z", fill "#41c3db" ]
            []
        , path [ d "M15.208 19.863l-.3-.183v3.578l.3-.186z", fill "#d9a371" ]
            []
        , path [ d "M14.909 23.258l2.983 1.847v-.36l-2.684-1.673z", fill "#a97d59" ]
            []
        , path [ d "M15.208 20.48l.979 3.202.68.423-1.202-3.963-.457-.279z", fill "#80cfdf" ]
            []
        , path [ d "M16.514 20.663l-.179-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M16.68 20.765l.9 3.784.108.068-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M19.132 25.909v-3.517l2.984 1.827v3.598z", fill "#41c3db" ]
            []
        , path [ d "M19.43 22.575l-.298-.183v3.579l.298-.187z", fill "#d9a371" ]
            []
        , path [ d "M19.132 25.97l2.984 1.846v-.36l-2.686-1.672z", fill "#a97d59" ]
            []
        , path [ d "M19.43 23.193l.98 3.201.68.423-1.203-3.962-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M20.737 23.375l-.179-.11v3.221l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M20.903 23.477l.899 3.784.11.068-.88-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M23.355 28.706V25.19l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M23.653 25.373l-.298-.183v3.578l.298-.186z", fill "#d9a371" ]
            []
        , path [ d "M23.355 28.768l2.984 1.846v-.36l-2.686-1.672z", fill "#a97d59" ]
            []
        , path [ d "M23.653 25.99l.981 3.201.68.424-1.204-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M24.961 26.173l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M25.127 26.275l.899 3.784.108.067-.878-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M14.909 28.545v-3.516l2.983 1.826v3.598z", fill "#41c3db" ]
            []
        , path [ d "M15.208 25.211l-.3-.182v3.578l.3-.187z", fill "#d9a371" ]
            []
        , path [ d "M14.909 28.607l2.983 1.846v-.36l-2.684-1.673z", fill "#a97d59" ]
            []
        , path [ d "M15.208 25.83l.979 3.2.68.424-1.202-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M16.514 26.011l-.179-.11v3.221l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M16.68 26.113l.9 3.784.108.068-.878-3.773z", fill "#80cfdf" ]
            []
        , path [ d "M19.132 31.257V27.74l2.984 1.826v3.598z", fill "#41c3db" ]
            []
        , path [ d "M19.43 27.923l-.298-.182v3.578l.298-.186z", fill "#d9a371" ]
            []
        , path [ d "M19.132 31.319l2.984 1.846v-.36l-2.686-1.672z", fill "#a97d59" ]
            []
        , path [ d "M19.43 28.541l.98 3.202.68.423-1.203-3.963-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M20.737 28.724l-.179-.11v3.22l.18.223z", fill "#a97d59" ]
            []
        , path [ d "M20.903 28.825l.899 3.785.11.067-.88-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M23.355 34.054v-3.516l2.984 1.827v3.597z", fill "#41c3db" ]
            []
        , path [ d "M23.653 30.722l-.298-.184v3.578l.298-.186z", fill "#d9a371" ]
            []
        , path [ d "M23.355 34.116l2.984 1.846v-.36l-2.686-1.672z", fill "#a97d59" ]
            []
        , path [ d "M23.653 31.339l.981 3.2.68.424L24.11 31l-.457-.28z", fill "#80cfdf" ]
            []
        , path [ d "M24.961 31.521l-.18-.11v3.22l.18.224z", fill "#a97d59" ]
            []
        , path [ d "M25.127 31.623l.899 3.784.108.067-.878-3.772z", fill "#80cfdf" ]
            []
        , path [ d "M22.292 18.582c0 1.357-.88 1.901-1.755 1.436-.875-.464-1.583-1.932-1.583-2.94 0-1.009.847-1.755 1.61-1.526.764.229 1.728 1.672 1.728 3.03", fill "#a97d59" ]
            []
        , path [ d "M22.147 18.51c.005 1.253-.797 1.776-1.609 1.343-.811-.432-1.464-1.765-1.457-2.715.007-.95.768-1.636 1.483-1.421.715.214 1.577 1.54 1.583 2.793", fill "#f59120" ]
            []
        , path [ d "M22.016 18.445c.008 1.158-.723 1.658-1.477 1.257-.753-.4-1.355-1.616-1.343-2.51.012-.894.697-1.526 1.367-1.324.67.201 1.444 1.42 1.453 2.577", fill "#dae4e3" ]
            []
        , path [ d "M21.933 18.405c.01 1.097-.677 1.58-1.393 1.2-.716-.38-1.286-1.522-1.272-2.38.014-.856.654-1.454 1.294-1.26.64.193 1.36 1.344 1.371 2.44", fill "#b8b8b9" ]
            []
        , path [ d "M21.781 18.333c.012.983-.595 1.432-1.24 1.091-.645-.34-1.155-1.35-1.14-2.137.017-.785.578-1.32 1.16-1.141.583.18 1.21 1.205 1.22 2.187", fill "#dae4e3" ]
            []
        , path [ d "M20.708 17.852c0 .125-.071.195-.158.157-.087-.039-.156-.169-.155-.29.002-.122.072-.193.158-.159.085.035.155.166.155.292", fill "#a97d59" ]
            []
        , path [ d "M21.56 18.226c0 .067-.041.101-.092.078-.368-.173-.526-.241-.84-.378-.043-.018-.077-.083-.077-.144 0-.061.035-.097.078-.08.312.13.468.195.836.36.05.023.093.097.094.164", fill "#a97d59" ]
            []
        , path [ d "M20.617 16.335c0-.052-.026-.103-.059-.114-.032-.01-.059.025-.06.077l-.011 1.436c0 .048.027.1.061.114.034.015.062-.013.062-.062l.007-1.45", fill "#a97d59" ]
            []
        ]
