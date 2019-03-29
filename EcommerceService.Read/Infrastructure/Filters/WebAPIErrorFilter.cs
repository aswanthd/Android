using Microsoft.ApplicationInsights;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Web;
using System.Web.Http.Filters;

namespace EcommerceService.Read.Infrastructure.Filters
{
    public class WebAPIErrorFilter : ExceptionFilterAttribute
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="WebAPIErrorFilter"/> class.
        /// </summary>
        public WebAPIErrorFilter()
        {
            ////  this.logger = ContainerConfig.Current.GetInstance<ILogger>();
        }

        /// <summary>
        /// On Exception.
        /// </summary>
        /// <param name="actionExecutedContext">Action Executed Context.</param>
        public override void OnException(HttpActionExecutedContext actionExecutedContext)
        {
            if (actionExecutedContext != null && actionExecutedContext.Exception != null)
            {
                var ai = new TelemetryClient();
                ai.TrackException(actionExecutedContext.Exception);
                var errorResponse = actionExecutedContext.Request.CreateErrorResponse(
              HttpStatusCode.InternalServerError,
              actionExecutedContext.Exception);

                actionExecutedContext.Response = errorResponse;
            }

            ////this.logger.Error("An unhandled exception has occured", actionExecutedContext.Exception);
        }
    }
}